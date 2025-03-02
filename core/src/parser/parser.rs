use core::str;
use std::{
    collections::HashMap,
    fs::File,
    io::{BufReader, Cursor, Read},
};

use colorful::Colorful;

use crate::{
    interpreter::value::{RecordKey, Value},
    lexer::{
        lexer::{self, LexResult},
        readers::{bytes_reader::BytesReader, stdin::StdinReader},
        token::{TokenInfo, TokenKind},
    },
    parser::ast::{ParamAst, TypeAst},
    util::{
        error::{BaseErrorExt, LineInfo},
        failable::Failable,
    },
};

use crate::lexer::lexer::Lexer;

use super::{
    ast::Ast,
    error::{ParseError, ParseOperatorError},
    op::{OperatorAssociativity, OperatorInfo, OperatorPosition, OperatorPrecedence},
};

/// Token predicates for parsing
mod pred {
    use crate::lexer::token::TokenKind;

    pub fn eof(t: &TokenKind) -> bool {
        matches!(t, TokenKind::EndOfFile)
    }

    /// Check if the token is an ignored token.
    /// These include:
    /// - `Newline`
    /// - `Comment`
    pub fn ignored(t: &TokenKind) -> bool {
        matches!(t, TokenKind::Comment(_) | TokenKind::Newline)
    }
}

//--------------------------------------------------------------------------------------//
//                                        Parser                                        //
//--------------------------------------------------------------------------------------//

/// A parse results is a list of AST nodes or a parse error.
pub type ParseResults = Result<Vec<Ast>, ParseError>;

/// A parse result is either an AST or a parse error.
pub type ParseResult = Result<Ast, ParseError>;

// A stream-lined parser for Lento with support for user-defined operators from function attributes and macros
#[derive(Clone)]
pub struct Parser<R>
where
    R: Read,
{
    lexer: Lexer<R>,
    /// A map of all defined operators in the parser indexed by their symbol.
    ///
    /// ## Note
    /// The parser will allow redefining operators with the same symbol **only if**:
    /// - They have different signatures
    /// - They have different positions
    /// - The symbol is a built-in operator that is overloadable
    operators: HashMap<String, Vec<OperatorInfo>>,
}

impl<R: Read> Parser<R> {
    pub fn new(lexer: Lexer<R>) -> Self {
        Self {
            lexer,
            operators: HashMap::new(),
        }
    }

    pub fn get_lexer(&mut self) -> &mut Lexer<R> {
        &mut self.lexer
    }

    pub fn move_lexer(self) -> Lexer<R> {
        self.lexer
    }

    pub fn get_content(&self) -> &[u8] {
        self.lexer.get_content()
    }

    pub fn move_content(self) -> Vec<u8> {
        self.lexer.move_content()
    }

    /// Define an operator in the parser.
    /// If the operator already exists with the same signature,
    pub fn define_op(&mut self, op: OperatorInfo) -> Failable<ParseOperatorError> {
        if let Some(existing) = self.get_op(&op.symbol) {
            if op.is_static && !existing.is_empty() {
                return Err(ParseOperatorError::NonStaticOperatorExists);
            }
            if existing.iter().any(|e| e.is_static) {
                return Err(ParseOperatorError::CannotOverrideStaticOperator);
            }
            if existing.iter().any(|e| e.position == op.position) {
                // TODO: Compare signatures instead of positions
                return Err(ParseOperatorError::PositionForSymbolExists);
            }
        }
        self.lexer.operators.insert(op.symbol.clone());
        self.operators
            .entry(op.symbol.clone())
            .or_default()
            .push(op);
        Ok(())
    }

    pub fn get_op(&self, symbol: &str) -> Option<&Vec<OperatorInfo>> {
        self.operators.get(symbol)
    }

    pub fn find_operator(
        &self,
        symbol: &str,
        pred: impl Fn(&OperatorInfo) -> bool,
    ) -> Option<&OperatorInfo> {
        self.get_op(symbol)
            .and_then(|ops| ops.iter().find(|op| pred(op)))
    }

    pub fn find_operator_pos(&self, symbol: &str, pos: OperatorPosition) -> Option<&OperatorInfo> {
        self.find_operator(symbol, |op| op.position == pos)
    }

    /// Parse a given number of expressions from the stream of tokens.
    /// Returns a global AST or a parse error.
    ///
    /// # Note
    /// If the parser encounters `EOF`, it will **ONLY add empty unit expressions** in the resulting AST.
    pub fn parse_exact(&mut self, count: usize) -> ParseResults {
        let mut ast = Vec::new();
        for _ in 0..count {
            match self.parse_one() {
                //? Ignore empty unit expressions,
                //? add top-level expressions to the global AST anyway
                Ok(expr) => ast.push(expr),
                Err(e) => return Err(e),
            }
        }
        Ok(ast)
    }

    /// Parse a global AST from the stream of tokens.
    /// A global AST is a list of **all** top-level AST nodes (expressions).
    pub fn parse_all(&mut self) -> ParseResults {
        let mut asts = Vec::new();
        loop {
            if let Ok(t) = self.lexer.peek_token_not(pred::ignored) {
                if pred::eof(&t.token) {
                    break;
                }
            }
            match self.parse_top_expr() {
                Ok(expr) => asts.push(expr),
                Err(e) => return Err(e),
            }
        }
        Ok(asts)
    }

    /// Parse **a single** expression from the stream of tokens.
    /// Returns an AST node or an error.
    /// If the first token is an EOF, then the parser will return an empty unit expression.
    ///
    /// # Note
    /// The parser will not necessarily consume all tokens from the stream.
    /// It will **ONLY** consume a whole complete expression.
    /// There may be remaining tokens in the stream after the expression is parsed.
    pub fn parse_one(&mut self) -> ParseResult {
        // Check if the next token is an EOF, then return an empty unit top-level expression
        if let Ok(t) = self.lexer.peek_token_not(pred::ignored) {
            if pred::eof(&t.token) {
                return Ok(Ast::Literal {
                    value: Value::Unit,
                    info: t.info,
                });
            }
        }
        self.parse_top_expr()
    }

    fn parse_literal(&mut self, token: &TokenKind, info: LineInfo) -> ParseResult {
        Ok(Ast::Literal {
            value: match token {
                TokenKind::Number(n) => Value::Number(n.clone()),
                TokenKind::String(s) => Value::String(s.clone()),
                TokenKind::Char(c) => Value::Char(*c),
                TokenKind::Boolean(b) => Value::Boolean(*b),
                _ => {
                    return Err(ParseError::new(
                        format!(
                            "Expected literal, but found {}",
                            token.to_string().light_red()
                        ),
                        info,
                    ));
                }
            },
            info,
        })
    }

    /// Parse a parenthesized function call.
    /// This function is called when the parser encounters an identifier followed by a left parenthesis.
    /// The parser will then attempt to parse the function call.
    /// ```lento
    /// func(a, b, c)
    /// ```
    /// If the next token is an assignment operator `=`, then the parser will attempt to parse a function definition.
    /// ```lento
    /// func(a, b, c) = expr
    /// ```
    fn parse_paren_call(&mut self, id: String, info: LineInfo) -> ParseResult {
        log::trace!("Parsing parenthesized function call: {}", id);
        let mut args = Vec::new();
        while let Ok(end) = self.lexer.peek_token(0) {
            if end.token == TokenKind::RightParen {
                break;
            }
            args.push(self.parse_top_expr()?);
            if let Ok(nt) = self.lexer.peek_token(0) {
                if nt.token == TokenKind::Comma {
                    self.lexer.next_token().unwrap();
                    continue;
                } else if nt.token == TokenKind::RightParen {
                    break;
                } else if let TokenKind::Identifier(param_name) = nt.token {
                    // Found (..., ty id) in an argument list.
                    // Check if `ty` is an identifier
                    if args.iter().all(|arg| matches!(arg, Ast::Identifier { .. })) {
                        let Ast::Identifier {
                            name: param_type,
                            info: param_info,
                        } = args.pop().unwrap()
                        else {
                            unreachable!("All arguments should be identifiers");
                        };
                        // Found a type identifier in a function call.
                        // Try to parse the rest as a function definition if all args are identifiers.
                        self.lexer.next_token().unwrap(); // Consume the `param_name` identifier
                                                          // Remove the last argument
                                                          // Expect the next token to be a comma or right paren
                        if let Ok(nt) = self.lexer.peek_token(0) {
                            if nt.token == TokenKind::Comma {
                                self.lexer.next_token().unwrap();
                            } else if nt.token == TokenKind::RightParen {
                                // Continue parsing the function definition
                                // The ) will be consumed by the `parse_func_def` function
                            } else {
                                return Err(ParseError::new(
                                    format!(
                                        "Expected {} or {}, but found {}",
                                        ",".yellow(),
                                        ")".yellow(),
                                        nt.token.to_string().light_red()
                                    ),
                                    info,
                                ));
                            }
                        }
                        return self.parse_func_def(
                            None,
                            id,
                            info,
                            args.iter()
                                .map(|arg| match arg {
                                    Ast::Identifier { name: id, info } => ParamAst {
                                        name: id.clone(),
                                        ty: None,
                                        info: info.clone(),
                                    },
                                    _ => unreachable!(),
                                })
                                // Also add the current parameter
                                .chain([ParamAst {
                                    name: param_name,
                                    ty: Some(TypeAst::Identifier(param_type, param_info.clone())),
                                    info: param_info,
                                }])
                                .collect(),
                        );
                    }
                }
            }
            if let Ok(nt) = self.lexer.peek_token(0) {
                return Err(ParseError::new(
                    format!(
                        "Expected {} or {}, but found {}",
                        ",".yellow(),
                        ")".yellow(),
                        nt.token.to_string().light_red()
                    ),
                    nt.info,
                ));
            } else {
                return Err(ParseError::new(
                    "Unexpected end of program".to_string(),
                    LineInfo::eof(info.start, self.lexer.current_index()),
                ));
            }
        }
        self.parse_expected(TokenKind::RightParen, ")")?;

        if let Some(func_def) = self.try_parse_func_def_no_types(&id, &info, &args) {
            return func_def;
        }

        log::trace!(
            "Parsed function call: {}({})",
            id,
            args.iter()
                .map(Ast::print_sexpr)
                .collect::<Vec<String>>()
                .join(", ")
        );
        Ok(syntax_sugar::roll_function_call(id, args, info))
        // Ok(Ast::Call(id, args))
    }

    /// If we encountered an expression like:
    /// ```lento
    /// func(a, b, c)
    /// ```
    /// Check if the subsequent token is an assignment operator `=`.
    /// If it is, then we have a function definition of the form:
    /// ```lento
    /// func(a, b, c) = expr
    /// ```
    fn try_parse_func_def_no_types(
        &mut self,
        func_name: &str,
        info: &LineInfo,
        args: &[Ast],
    ) -> Option<ParseResult> {
        if let Ok(ref nt) = self.lexer.peek_token(0) {
            if let TokenKind::Op(ref op) = nt.token {
                if op == "=" {
                    // If all arguments are identifiers, then this is a function definition
                    if args.iter().all(|arg| matches!(arg, Ast::Identifier { .. })) {
                        log::trace!(
                            "Parsed function definition: {}({:?}) -> {:?}",
                            func_name,
                            args,
                            nt
                        );
                        self.lexer.next_token().unwrap();
                        let body = match self.parse_top_expr() {
                            Ok(body) => body,
                            Err(err) => {
                                log::warn!("Failed to parse function body: {}", err.base().message);
                                return Some(Err(err));
                            }
                        };
                        let params = args
                            .iter()
                            .map(|arg| match arg {
                                Ast::Identifier { name: id, info } => ParamAst {
                                    name: id.clone(),
                                    ty: None,
                                    info: info.clone(),
                                },
                                _ => unreachable!(),
                            })
                            .collect::<Vec<_>>();
                        // Roll functions into single param definitions
                        let function = syntax_sugar::roll_function_definition(params, body);
                        let assign_info = info.join(function.info());
                        return Some(Ok(Ast::Assignment {
                            target: Box::new(Ast::Identifier {
                                name: func_name.to_string(),
                                info: info.clone(),
                            }),
                            expr: Box::new(function),
                            info: assign_info,
                        }));
                    }
                }
            }
        }
        None
    }

    /// If we encountered an expression like:
    /// ```lento
    /// int func(
    /// ```
    /// or
    /// ```lento
    /// func(int a
    /// ```
    /// Then we have a function definition of the form:
    /// ```lento
    /// int func(...) = expr
    /// ```
    /// or
    /// ```lento
    /// func(int a, ...) = expr
    /// ```
    fn parse_func_def(
        &mut self,
        ret_type: Option<String>,
        name: String,
        info: LineInfo,
        parsed_params: Vec<ParamAst>,
    ) -> ParseResult {
        log::trace!("Parsing function definition: {} -> {:?}", name, ret_type);
        let mut params = parsed_params;
        while let Ok(end) = self.lexer.peek_token(0) {
            if end.token == TokenKind::RightParen {
                break;
            }
            let ty = match self.try_parse_type() {
                Some(Ok(t)) => t,
                Some(Err(err)) => {
                    return Err(ParseError::new(
                        format!("Failed to parse function parameter: {}", err.base().message),
                        LineInfo::eof(info.end, self.lexer.current_index()),
                    ));
                }
                None => {
                    return Err(ParseError::new(
                        format!(
                            "Expected type identifier, but found {}",
                            end.token.to_string().light_red()
                        ),
                        info,
                    ));
                }
            };
            let (param_name, param_info) = match self.lexer.next_token() {
                Ok(t) => match t.token {
                    TokenKind::Identifier(id) => (id, t.info),
                    _ => {
                        return Err(ParseError::new(
                            format!(
                                "Expected parameter name, but found {}",
                                t.token.to_string().light_red()
                            ),
                            info,
                        ));
                    }
                },
                Err(err) => {
                    return Err(ParseError::new(
                        format!("Failed to parse parameter name: {}", err.inner.message),
                        LineInfo::eof(info.end, self.lexer.current_index()),
                    ));
                }
            };
            params.push(ParamAst {
                name: param_name,
                ty: Some(ty),
                info: param_info,
            });
            if let Ok(nt) = self.lexer.peek_token(0) {
                if nt.token == TokenKind::Comma {
                    self.lexer.next_token().unwrap();
                    continue;
                } else if nt.token == TokenKind::RightParen {
                    break;
                }
            }
            if let Ok(nt) = self.lexer.peek_token(0) {
                return Err(ParseError::new(
                    format!(
                        "Expected {} or {}, but found {}",
                        ",".yellow(),
                        ")".yellow(),
                        nt.token.to_string().light_red()
                    ),
                    nt.info,
                ));
            } else {
                return Err(ParseError::new(
                    "Unexpected end of program".to_string(),
                    LineInfo::eof(info.end, self.lexer.current_index()),
                ));
            }
        }
        self.parse_expected(TokenKind::RightParen, ")")?;
        self.parse_expected(TokenKind::Op("=".into()), "=")?;
        let body = self.parse_top_expr()?;
        log::trace!(
            "Parsed function definition: {}({:?}) -> {:?}",
            name,
            params,
            body
        );
        let function = syntax_sugar::roll_function_definition(params, body);
        let assign_info = info.join(function.info());
        Ok(Ast::Assignment {
            target: Box::new(Ast::Identifier { name, info }),
            expr: Box::new(function),
            info: assign_info,
        })
    }

    fn try_parse_type(&mut self) -> Option<Result<TypeAst, ParseError>> {
        if let Ok(t) = self.lexer.peek_token(0) {
            match t.token {
                TokenKind::Identifier(id) => {
                    self.lexer.next_token().unwrap();
                    Some(Ok(TypeAst::Identifier(id, t.info)))
                }
                _ => None,
            }
        } else {
            None
        }
    }

    /// Parses the fields of a record from the lexer.
    ///
    /// This function attempts to parse a record by first performing a soft parse to check if the record
    /// is empty or if it is a block. If a valid key and a colon are found, it continues to parse the
    /// fields more strictly.
    ///
    /// # Returns
    /// - `Some(Ok(Vec<(RecordKeyAst, Ast)>))` if the record is successfully parsed.
    /// - `Some(Err(ParseError))` if there is an error during parsing.
    /// - `None` if the input does not represent a record.
    ///
    /// # Errors
    /// This function returns a `ParseError` if it encounters unexpected tokens or if it fails to parse
    /// the expected tokens.
    ///
    /// # Examples
    /// ```
    /// let mut parser = Parser::new(lexer);
    /// if let Some(result) = parser.parse_record_fields() {
    ///     match result {
    ///         Ok(fields) => println!("Parsed fields: {:?}", fields),
    ///         Err(err) => eprintln!("Parse error: {:?}", err),
    ///     }
    /// } else {
    ///     println!("Not a record.");
    /// }
    /// ```
    #[allow(clippy::type_complexity)]
    fn parse_record_fields(&mut self, first_info: &LineInfo) -> Option<Result<Ast, ParseError>> {
        let mut fields = Vec::new();
        // Initial soft parse to check if the record is empty
        // Or if it is a block
        if let Ok(t) = self.lexer.peek_token(0) {
            let key = match t.token {
                TokenKind::RightBrace => {
                    self.lexer.next_token().unwrap();
                    return Some(Ok(Ast::Record {
                        fields,
                        info: t.info,
                    })); // Empty record
                }
                TokenKind::Identifier(id) => RecordKey::String(id),
                TokenKind::Number(n) => RecordKey::Number(n),
                TokenKind::String(s) => RecordKey::String(s),
                TokenKind::Char(c) => RecordKey::Char(c),
                _ => return None, // Not a record
            };
            if let Ok(t) = self.lexer.peek_token(1) {
                if t.token != TokenKind::Colon {
                    return None; // Not a record
                }
            }
            // If we found both a valid key and a colon, we found a record!
            self.lexer.next_token().unwrap();
            self.parse_expected(TokenKind::Colon, ":").ok()?;
            let value = self.parse_top_expr().ok()?;
            fields.push((key, value));
            if let Ok(t) = self.lexer.next_token() {
                match t.token {
                    TokenKind::Comma => (), // Continue parsing
                    TokenKind::RightBrace => {
                        return Some(Ok(Ast::Record {
                            fields,
                            info: t.info,
                        }))
                    } // Just a single field
                    _ => {
                        return Some(Err(ParseError::new(
                            format!(
                                "Expected {} or {}, but found {}",
                                ",".yellow(),
                                "}".yellow(),
                                t.token.to_string().light_red()
                            ),
                            t.info,
                        )));
                    }
                }
            }
        }
        let mut last_info = LineInfo::default();
        // Parse the rest of the fields more strictly
        while let Ok(t) = self.lexer.next_token() {
            if t.token == TokenKind::RightBrace {
                last_info = t.info;
                break;
            }
            let key = match t.token {
                TokenKind::Identifier(id) => RecordKey::String(id),
                TokenKind::Number(n) => RecordKey::Number(n),
                TokenKind::String(s) => RecordKey::String(s),
                TokenKind::Char(c) => RecordKey::Char(c),
                _ => {
                    return Some(Err(ParseError::new(
                        format!(
                            "Expected record key, but found {}",
                            t.token.to_string().light_red()
                        ),
                        t.info,
                    )));
                }
            };
            let _ = Some(self.parse_expected(TokenKind::Colon, ":"))?;
            let value = match self.parse_top_expr() {
                Ok(value) => value,
                Err(err) => return Some(Err(err)),
            };
            fields.push((key, value));
            if let Ok(t) = self.lexer.next_token() {
                match t.token {
                    TokenKind::Comma => continue,
                    TokenKind::RightBrace => {
                        last_info = t.info;
                        break;
                    }
                    _ => {
                        return Some(Err(ParseError::new(
                            format!(
                                "Expected {} or {}, but found {}",
                                ",".yellow(),
                                "}".yellow(),
                                t.token.to_string().light_red()
                            ),
                            t.info,
                        )));
                    }
                }
            }
        }
        Some(Ok(Ast::Record {
            fields,
            info: first_info.join(&last_info),
        }))
    }

    fn parse_primary(&mut self) -> ParseResult {
        match self.lexer.expect_next_token_not(pred::ignored) {
            Ok(t) => {
                Ok(match t.token {
                    lit if lit.is_literal() => self.parse_literal(&lit, t.info)?,
                    TokenKind::Identifier(id) => {
                        // Check if function call
                        if let Ok(t) = self.lexer.peek_token(0) {
                            if t.token
                                == (TokenKind::LeftParen {
                                    is_function_call: true,
                                })
                            {
                                self.lexer.next_token().unwrap(); // Consume the left paren
                                return self.parse_paren_call(id, t.info);
                            }
                        }
                        // Check if function definition
                        if let Ok(t) = self.lexer.peek_token(0) {
                            if let TokenKind::Identifier(name) = t.token {
                                if let Ok(t) = self.lexer.peek_token(1) {
                                    if t.token
                                        == (TokenKind::LeftParen {
                                            is_function_call: true,
                                        })
                                    {
                                        self.lexer.next_token().unwrap(); // Consume the name identifier
                                        self.lexer.next_token().unwrap(); // Consume the left paren
                                                                          // Start parsing the params and body of the function
                                        return self.parse_func_def(Some(id), name, t.info, vec![]);
                                    }
                                }
                            } else if let TokenKind::LeftParen { .. } = t.token {
                                self.lexer.next_token().unwrap(); // Consume the left paren
                                return self.parse_paren_call(id, t.info);
                            }
                        }
                        Ast::Identifier {
                            name: id,
                            info: t.info,
                        }
                    }
                    TokenKind::Op(op) => {
                        // TODO: Don't lookup operators in the parser, do this in the type checker!
                        if let Some(op) = self.find_operator_pos(&op, OperatorPosition::Prefix) {
                            let op = op.clone();
                            let rhs = self.parse_primary()?;
                            Ast::Unary {
                                op_info: op.clone(),
                                expr: Box::new(rhs),
                                info: t.info,
                            }
                        } else {
                            return Err(ParseError::new(
                                format!("Expected prefix operator, but found {}", op.light_red()),
                                t.info,
                            ));
                        }
                    }
                    start if start.is_grouping_start() => {
                        match start {
                            // Tuples, Units and Parentheses: ()
                            TokenKind::LeftParen {
                                is_function_call: false,
                            } => {
                                // Tuples are defined by a comma-separated list of expressions
                                let mut explicit_single = false;
                                let mut exprs = Vec::new();
                                while let Ok(end) = self.lexer.peek_token(0) {
                                    if end.token == TokenKind::RightParen {
                                        break;
                                    }
                                    exprs.push(self.parse_top_expr()?);
                                    if let Ok(nt) = self.lexer.peek_token(0) {
                                        if nt.token == TokenKind::Comma {
                                            self.lexer.next_token().unwrap();
                                            if self.lexer.peek_token(0).unwrap().token
                                                == TokenKind::RightParen
                                            {
                                                explicit_single = true;
                                                // Break in the next iteration
                                            }
                                            continue;
                                        } else if nt.token == TokenKind::RightParen {
                                            break;
                                        }
                                    }
                                    if let Ok(nt) = self.lexer.peek_token(0) {
                                        return Err(ParseError::new(
                                            format!(
                                                "Expected {} or {}, but found {}",
                                                ",".yellow(),
                                                ")".yellow(),
                                                nt.token.to_string().light_red()
                                            ),
                                            nt.info,
                                        ));
                                    } else {
                                        return Err(ParseError::new(
                                            "Unexpected end of program".to_string(),
                                            LineInfo::eof(t.info.end, self.lexer.current_index()),
                                        ));
                                    }
                                }
                                let end = self.parse_expected(TokenKind::RightParen, ")")?;
                                if exprs.len() == 1 && !explicit_single {
                                    exprs.pop().unwrap()
                                } else {
                                    Ast::Tuple {
                                        exprs,
                                        info: t.info.join(&end.info),
                                    }
                                }
                            }
                            // Records and Blocks: {}
                            TokenKind::LeftBrace => {
                                // Try to parse as record
                                if let Some(res) = self.parse_record_fields(&t.info) {
                                    res?
                                } else {
                                    // Parse as block
                                    let mut exprs = Vec::new();
                                    while let Ok(end) = self.lexer.peek_token(0) {
                                        if end.token == TokenKind::RightBrace {
                                            break;
                                        }
                                        exprs.push(self.parse_top_expr()?);
                                    }
                                    let last = self.parse_expected(TokenKind::RightBrace, "}")?;
                                    Ast::Block {
                                        exprs,
                                        info: t.info.join(&last.info),
                                    }
                                }
                            }
                            // Lists: []
                            TokenKind::LeftBracket => {
                                let mut exprs = Vec::new();
                                while let Ok(end) = self.lexer.peek_token(0) {
                                    if end.token == TokenKind::RightBracket {
                                        break;
                                    }
                                    exprs.push(self.parse_top_expr()?);
                                    if let Ok(nt) = self.lexer.peek_token(0) {
                                        if nt.token == TokenKind::Comma {
                                            self.lexer.next_token().unwrap();
                                            continue;
                                        } else if nt.token == TokenKind::RightBracket {
                                            break;
                                        }
                                    }
                                    if let Ok(nt) = self.lexer.peek_token(0) {
                                        return Err(ParseError::new(
                                            format!(
                                                "Expected {} or {}, but found {}",
                                                ",".yellow(),
                                                "]".yellow(),
                                                nt.token.to_string().light_red()
                                            ),
                                            nt.info,
                                        ));
                                    } else {
                                        return Err(ParseError::new(
                                            "Unexpected end of program".to_string(),
                                            LineInfo::eof(t.info.end, self.lexer.current_index()),
                                        ));
                                    }
                                }
                                let last = self.parse_expected(TokenKind::RightBracket, "]")?;
                                Ast::List {
                                    exprs,
                                    info: t.info.join(&last.info),
                                }
                            }
                            _ => unreachable!(),
                        }
                    }
                    _ => {
                        return Err(ParseError::new(
                            format!(
                                "Expected primary expression, but found {}",
                                t.token.to_string().light_red()
                            ),
                            t.info,
                        ));
                    }
                })
            }
            Err(err) => Err(ParseError::new(
                format!(
                    "Expected primary expression, but failed due to: {}",
                    err.inner.message
                ),
                err.inner.info,
            )),
        }
    }

    /// Check if to continue parsing the next expression in the sequence
    /// based on the precedence of the next operator.
    ///
    /// ## Returns
    /// - `Some(op)`: If the next token is an infix binary operator that either:
    ///     - has a precedence **greater than** `min_prec`
    ///     - is **right-associative** with a precedence **greater than or equal** to `min_prec`
    ///     - `allow_eq` is `true` and precedence **equal** to `min_prec`
    /// - `None`: If the next token is either:
    ///     - **not an infix operator**
    ///     - its **precedence is lower than** `min_prec`
    ///     - it is a **terminator**
    fn check_op(
        &self,
        nt: &LexResult,
        min_prec: OperatorPrecedence,
        allow_eq: bool,
    ) -> Option<OperatorInfo> {
        let t = nt.as_ref().ok()?;
        let op = if let TokenKind::Op(op) = &t.token {
            op
        } else {
            return None;
        };
        let op = self.find_operator(op, |op| {
            op.position == OperatorPosition::Infix
                || op.position == OperatorPosition::InfixAccumulate
        })?;
        let is_infix = op.position.is_infix();
        let is_greater = op.precedence > min_prec;
        let is_right_assoc = op.associativity == OperatorAssociativity::Right;
        let is_equal = op.precedence == min_prec;
        if is_infix && (is_greater || ((is_right_assoc || allow_eq) && is_equal)) {
            Some(op.clone())
        } else {
            None
        }
    }

    fn next_prec(curr_op: &OperatorInfo, next_op: &OperatorInfo) -> OperatorPrecedence {
        curr_op.precedence + (next_op.precedence > curr_op.precedence) as OperatorPrecedence
    }

    /// Parse an expression with a given left-hand side and minimum precedence level
    /// using the operator precedence parsing (pratt parsing) algorithm.
    ///
    /// ## Arguments
    /// - `lhs` The left-hand side of the expression
    /// - `min_prec` The minimum precedence of the expression
    ///
    /// ## Returns
    /// The parsed expression or a parse error if the expression could not be parsed
    ///
    /// ## Algorithm
    /// See:
    /// - https://en.wikipedia.org/wiki/Operator-precedence_parser
    /// - https://matklad.github.io/2020/04/13/simple-but-powerful-pratt-parsing.html
    /// - https://eli.thegreenplace.net/2012/08/02/parsing-expressions-by-precedence-climbing
    /// - https://www.engr.mun.ca/~theo/Misc/exp_parsing.htm
    /// - https://crockford.com/javascript/tdop/tdop.html
    fn parse_expr(&mut self, lhs: Ast, min_prec: OperatorPrecedence) -> ParseResult {
        let mut expr = lhs;
        while let Some(curr_op) = {
            let nt = self.lexer.peek_token(0);
            self.check_op(&nt, min_prec, false)
        } {
            let curr_info = self.lexer.next_token().unwrap().info; // Consume the operator token
            if curr_op.position.is_accumulate() {
                expr = self.parse_expr_accum(&curr_op, expr, curr_info)?;
                continue;
            }
            let mut rhs = self.parse_primary()?;
            while let Some(next_op) = {
                let nt = self.lexer.peek_token(0);
                self.check_op(&nt, curr_op.precedence, false)
            } {
                rhs = self.parse_expr(rhs, Self::next_prec(&curr_op, &next_op))?;
            }
            if let Some(desugar) = syntax_sugar::try_binary(&expr, &curr_op, &rhs) {
                expr = desugar;
            } else {
                let info = expr.info().join(rhs.info());
                expr = Ast::Binary {
                    lhs: Box::new(expr),
                    op_info: curr_op.clone(),
                    rhs: Box::new(rhs),
                    info,
                };
            }
        }
        Ok(expr)
    }

    /// Expect the parser state to be at the end of [expr, op] sequence.
    /// Next token should be a new expression or a terminator.
    fn parse_expr_accum(&mut self, op: &OperatorInfo, first: Ast, info: LineInfo) -> ParseResult {
        let mut exprs = vec![first];
        while let Ok(t) = self.lexer.peek_token_not(pred::ignored) {
            if op.allow_trailing && t.token.is_terminator() {
                break;
            }

            // Parse the next nested expression in the sequence
            let lhs = self.parse_primary()?;
            exprs.push(self.parse_expr(lhs, op.precedence)?);

            // Expect the next token to be the same operator or another expression
            let nt = self.lexer.peek_token_not(pred::ignored);
            if let Some(next_op) = self.check_op(&nt, op.precedence, true) {
                if !next_op.eq(op) {
                    break;
                }
                self.lexer.read_next_token_not(pred::ignored).unwrap(); // Consume the operator token
            } else {
                break;
            }
        }
        let info = info.join(exprs.last().unwrap().info());
        Ok(Ast::Accumulate {
            op_info: op.clone(),
            exprs,
            info,
        })
    }

    /// Parse a top-level expression.
    fn parse_top_expr(&mut self) -> ParseResult {
        let lhs = self.parse_primary()?;
        let expr = self.parse_expr(lhs, 0);
        let nt = self.lexer.peek_token(0);
        if let Ok(t) = nt {
            if t.token.is_top_level_terminal(false) {
                self.lexer.next_token().unwrap();
            }
        }
        expr
    }

    fn parse_expected(
        &mut self,
        expected_token: TokenKind,
        symbol: &'static str,
    ) -> Result<TokenInfo, ParseError> {
        match self.lexer.expect_next_token_not(pred::ignored) {
            Ok(t) if t.token == expected_token => Ok(t),
            Ok(t) => Err(ParseError::new(
                format!(
                    "Expected {} but found {}",
                    symbol.yellow(),
                    t.token.to_string().light_red()
                ),
                t.info,
            )),
            Err(err) => Err(ParseError::new(
                format!(
                    "Expected {}, but failed due to: {}",
                    symbol.yellow(),
                    err.inner.message
                ),
                err.inner.info,
            )),
        }
    }
}

mod syntax_sugar {
    use malachite::{num::basic::traits::Zero, Integer, Rational};

    use crate::interpreter::number::{FloatingPoint, Number, NumberCasting};

    use super::*;

    pub fn try_literal_fraction(lhs: &Number, rhs: &Number, info: LineInfo) -> Option<Ast> {
        let lhs = match lhs {
            Number::UnsignedInteger(lhs) => lhs.to_signed(),
            Number::SignedInteger(lhs) => lhs.clone(),
            _ => return None,
        }
        .to_bigint();
        let rhs = match rhs {
            Number::UnsignedInteger(rhs) => rhs.to_signed(),
            Number::SignedInteger(rhs) => rhs.clone(),
            _ => return None,
        }
        .to_bigint();
        if rhs.cmp(&Integer::ZERO) == std::cmp::Ordering::Equal {
            return None;
        }
        Some(Ast::Literal {
            value: Value::Number(Number::FloatingPoint(
                FloatingPoint::FloatBig(Rational::from_integers(lhs, rhs)).optimize(),
            )),
            info,
        })
    }

    pub fn try_binary(lhs: &Ast, op: &OperatorInfo, rhs: &Ast) -> Option<Ast> {
        match (lhs, op, rhs) {
            (
                Ast::Literal {
                    value: Value::Number(lhs),
                    info: left_info,
                },
                OperatorInfo { name, symbol, .. },
                Ast::Literal {
                    value: Value::Number(rhs),
                    info: right_info,
                },
            ) if name == "div" && symbol == "/" => {
                try_literal_fraction(lhs, rhs, left_info.join(right_info))
            }
            _ => None,
        }
    }

    /// Takes a function name, a list of parameters and a body and rolls them into a single assignment expression.
    /// Parameters are rolled into a nested function definition.
    /// All parameters are sorted like:
    /// ```lento
    /// func(a, b, c) = expr
    /// ```
    /// becomes:
    /// ```lento
    /// func = a -> b -> c -> expr
    /// ```
    ///
    /// # Arguments
    /// - `func_name` The name of the function
    /// - `params` A list of parameters in left-to-right order: `a, b, c`
    /// - `body` The body of the function
    pub fn roll_function_definition(params: Vec<ParamAst>, body: Ast) -> Ast {
        assert!(!params.is_empty(), "Expected at least one parameter");
        let info = body.info().join(params.last().map(|p| &p.info).unwrap());
        let mut params = params.iter().rev();
        let mut function = Ast::FunctionDef {
            param: params.next().unwrap().clone(),
            body: Box::new(body),
            return_type: None,
            info,
        };
        for param in params {
            function = Ast::FunctionDef {
                info: function.info().join(&param.info),
                param: param.clone(),
                body: Box::new(function),
                return_type: None,
            };
        }
        function
    }

    pub fn roll_function_call(name: String, args: Vec<Ast>, start_info: LineInfo) -> Ast {
        let last_info = args
            .last()
            .map(|a| a.info().clone())
            .unwrap_or(start_info.clone());
        let call_info = start_info.join(&last_info);
        let mut args = args.into_iter();
        let mut call = Ast::FunctionCall {
            expr: Box::new(Ast::Identifier {
                name,
                info: call_info.clone(),
            }),
            arg: Box::new(args.next().unwrap()),
            info: call_info.clone(),
        };
        for arg in args {
            let arg_info = call_info.join(arg.info());
            call = Ast::FunctionCall {
                expr: Box::new(call),
                arg: Box::new(arg),
                info: arg_info,
            };
        }
        call
    }
}

//--------------------------------------------------------------------------------------//
//                               Parser Factory Functions                               //
//--------------------------------------------------------------------------------------//

pub fn from_file(file: File) -> Parser<BufReader<File>> {
    Parser::new(lexer::from_file(file))
}

pub fn from_string(source: String) -> Parser<Cursor<String>> {
    Parser::new(lexer::from_string(source))
}

pub fn from_str(source: &str) -> Parser<BytesReader<'_>> {
    Parser::new(lexer::from_str(source))
}

pub fn from_stdin() -> Parser<StdinReader> {
    Parser::new(lexer::from_stdin())
}

pub fn from_stream<R: Read>(reader: R) -> Parser<R> {
    Parser::new(lexer::from_stream(reader))
}
