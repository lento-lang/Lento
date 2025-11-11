use super::{
    ast::Ast,
    error::{ParseError, ParserOpError},
    op::{
        prec::{COMMA_PREC, FUNCTION_APP_PREC},
        OpAssoc, OpInfo, OpPos, OpPrec,
    },
    utils,
};
use crate::{
    interpreter::value::{RecordKey, Value},
    lexer::{
        lexer::{self, Lexer},
        readers::{bytes_reader::BytesReader, stdin::StdinReader},
        token::{TokenInfo, TokenKind},
    },
    parser::op::prec,
    util::{
        error::{BaseErrorExt, LineInfo},
        failable::Failable,
    },
};
use colorful::Colorful;
use std::{
    collections::HashMap,
    fs::File,
    io::{BufReader, Cursor, Read},
};

/// Token predicates for parsing
mod pred {
    use crate::lexer::token::TokenKind;

    pub fn eof(t: &TokenKind) -> bool {
        matches!(t, TokenKind::EndOfFile)
    }

    /// Check if the token is an ignore token.
    /// These include:
    /// - `Newline`
    /// - `Comment`
    pub fn ignore(t: &TokenKind) -> bool {
        matches!(t, TokenKind::Comment(_) | TokenKind::Newline)
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

//--------------------------------------------------------------------------------------//
//                                        Parser                                        //
//--------------------------------------------------------------------------------------//

pub(crate) const COMMA_SYM: &str = ",";
pub(crate) const ASSIGNMENT_SYM: &str = "=";
pub(crate) const MEMBER_ACCESS_SYM: &str = ".";

/// Default operators used in the language grammar and required for parsing. \
/// These operators are defined in the parser and are required to produce valid ASTs. \
/// The binary operators are replaced with `Ast` nodes by `syntax_sugar::specialize` after parsing a `parse_top_expr` expression.
/// - `semicolon`: `;` - Used to separate statements becomes an `Ast::Block` node.
/// - `comma`: `,` - Used to separate expressions in tuples and lists becomes an `Ast::Tuple` or `Ast::List` node.
/// - `assignment`: `=` - Used to assign values to variables becomes an `Ast::Assignment` node.
/// - `member access`: `.` - Used to access members of records becomes an `Ast::MemberAccess` node.
pub fn intrinsic_operators() -> Vec<OpInfo> {
    vec![
        OpInfo {
            symbol: COMMA_SYM.to_string(),
            position: OpPos::Infix,
            precedence: prec::COMMA_PREC,
            associativity: OpAssoc::Left,
            allow_trailing: true,
        },
        OpInfo {
            symbol: ASSIGNMENT_SYM.to_string(),
            position: OpPos::Infix,
            precedence: prec::ASSIGNMENT_PREC,
            associativity: OpAssoc::Right,
            allow_trailing: false,
        },
        OpInfo {
            symbol: MEMBER_ACCESS_SYM.to_string(),
            position: OpPos::Infix,
            precedence: prec::MEMBER_ACCESS_PREC,
            associativity: OpAssoc::Left,
            allow_trailing: false,
        },
    ]
}

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
    /// - The symbol is a built-in operator that is overload:able
    operators: HashMap<String, Vec<OpInfo>>,
}

impl<R: Read> Parser<R> {
    pub fn new(lexer: Lexer<R>) -> Self {
        Self {
            lexer,
            operators: HashMap::new(),
        }
        .init_default_operators()
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

    /// Initialize the parser with default operators.
    fn init_default_operators(mut self) -> Self {
        intrinsic_operators().into_iter().for_each(|op| {
            self.define_op(op)
                .expect("Failed to define default operator")
        });
        self
    }

    /// Define an operator in the parser.
    /// If the operator already exists with the same signature,
    pub fn define_op(&mut self, op: OpInfo) -> Failable<ParserOpError> {
        if let Some(existing) = self.get_op(&op.symbol) {
            if existing.iter().any(|e| e.position == op.position) {
                return Err(ParserOpError::AlreadyExists);
            }
        }
        self.lexer.add_operator(op.symbol.clone());
        self.operators
            .entry(op.symbol.clone())
            .or_default()
            .push(op);
        Ok(())
    }

    pub fn get_op(&self, symbol: &str) -> Option<&Vec<OpInfo>> {
        self.operators.get(symbol)
    }

    pub fn find_operator(&self, symbol: &str, pred: impl Fn(&OpInfo) -> bool) -> Option<&OpInfo> {
        self.get_op(symbol)
            .and_then(|ops| ops.iter().find(|op| pred(op)))
    }

    pub fn find_operator_pos(&self, symbol: &str, pos: OpPos) -> Option<&OpInfo> {
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
                //? Add top-level expressions to the global AST anyway
                Ok(expr) => ast.push(expr),
                Err(e) => return Err(e),
            }
        }
        Ok(ast)
    }

    fn parse_expected(
        &mut self,
        condition: impl FnOnce(&TokenKind) -> bool,
        symbol: &'static str,
    ) -> Result<TokenInfo, ParseError> {
        match self.lexer.expect_next_token_not(pred::ignore) {
            Ok(t) if condition(&t.token) => Ok(t),
            Ok(t) => Err(ParseError::new(
                format!(
                    "Expected {} but found {}",
                    symbol.yellow(),
                    t.token.to_string().light_red()
                ),
                t.info.clone(),
            )
            .with_label(format!("This should be a {}", symbol), t.info)),
            Err(err) => Err(ParseError::new(
                format!("Expected {}", symbol.yellow(),),
                err.info().clone(),
            )
            .with_label(err.message().to_owned(), err.info().clone())),
        }
    }

    fn parse_expected_eq(
        &mut self,
        expected_token: TokenKind,
        symbol: &'static str,
    ) -> Result<TokenInfo, ParseError> {
        self.parse_expected(|t| t == &expected_token, symbol)
    }

    // ======================================== EXPRESSION PARSING ======================================== //

    /// Parse a literal `Value` from the lexer.
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
                        info.clone(),
                    )
                    .with_label("This is not a valid literal.".to_string(), info));
                }
            },
            info,
        })
    }

    /// Parse a tuple from the lexer.
    ///
    /// ## Examples
    /// ```ignore
    /// ()
    /// (1, 2)
    /// (1, 2, 3)
    /// ```
    fn parse_tuple(&mut self) -> ParseResult {
        // Check if the next token is a right parenthesis `)`, then return an empty tuple
        if let Ok(t) = self.lexer.peek_token(0) {
            if t.token == TokenKind::RightParen {
                self.lexer.next_token().unwrap();
                return Ok(Ast::unit(t.info));
            }
        }
        log::trace!("Parsing elements...");
        let tuple = self.parse_top_expr()?;
        log::trace!("Parsed tuple elements: {:?}", tuple);
        self.parse_expected_eq(TokenKind::RightParen, ")")?;
        Ok(tuple)
    }

    fn parse_record_or_block(&mut self, start_info: LineInfo) -> ParseResult {
        // Try to parse as record
        if let Some(res) = self.parse_record_fields(&start_info) {
            res
        } else {
            // Parse as block
            let mut exprs = Vec::new();
            while let Ok(end) = self.lexer.peek_token(0) {
                if end.token == TokenKind::RightBrace {
                    break;
                }
                exprs.push(self.parse_top_expr()?);
            }
            let last = self.parse_expected_eq(TokenKind::RightBrace, "}")?;
            Ok(Ast::Block {
                exprs,
                info: start_info.join(&last.info),
            })
        }
    }

    fn parse_list(&mut self, start_info: LineInfo) -> ParseResult {
        let mut exprs = Vec::new();
        while let Ok(end) = self.lexer.peek_token(0) {
            if end.token == TokenKind::RightBracket {
                break;
            }
            exprs.push(self.parse_expr(COMMA_PREC)?);
            if let Ok(nt) = self.lexer.peek_token(0) {
                if nt.token == TokenKind::Op(COMMA_SYM.to_string()) {
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
                    nt.info.clone(),
                )
                .with_label(
                    "This should be either a comma or a right bracket".to_string(),
                    nt.info,
                ));
            } else {
                return Err(ParseError::new(
                    "Unexpected end of program".to_string(),
                    LineInfo::eof(start_info.end, self.lexer.current_index()),
                ));
            }
        }
        let last = self.parse_expected_eq(TokenKind::RightBracket, "]")?;
        Ok(Ast::List {
            exprs,
            info: start_info.join(&last.info),
        })
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
    /// ```d
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
    fn parse_record_fields(&mut self, start_info: &LineInfo) -> Option<Result<Ast, ParseError>> {
        let mut last_info = LineInfo::default();
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
                // TokenKind::Number(n) => RecordKey::Number(n),
                TokenKind::String(s) => RecordKey::String(s),
                TokenKind::Char(c) => RecordKey::String(c.to_string()),
                _ => return None, // Not a record
            };
            let Ok(t) = self.lexer.peek_token(1) else {
                return None;
            };
            if t.token != TokenKind::Colon {
                return None; // Not a record
            }

            // If we found both a valid key and a colon, we found a record!
            self.lexer.next_token().unwrap(); // Consume the key
            self.lexer.next_token().unwrap(); // Consume the colon
            let value = match self.parse_expr(COMMA_PREC) {
                Ok(value) => value,
                Err(err) => return Some(Err(err)),
            };
            fields.push((key, value));
            if let Ok(t) = self.lexer.next_token() {
                match t.token {
                    TokenKind::Op(op) if op == COMMA_SYM => {
                        last_info = t.info;
                    }
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
                            start_info.join(&t.info),
                        )
                        .with_label(
                            "This should be either a comma or a right brace".to_string(),
                            t.info,
                        )));
                    }
                }
            }
        }
        // Parse the rest of the fields more strictly
        while let Ok(t) = self.lexer.next_token() {
            if t.token == TokenKind::RightBrace {
                last_info = t.info;
                break;
            }
            let key = match t.token {
                TokenKind::Identifier(id) => RecordKey::String(id),
                // TokenKind::Number(n) => RecordKey::Number(n),
                TokenKind::String(s) => RecordKey::String(s),
                TokenKind::Char(c) => RecordKey::String(c.to_string()),
                _ => {
                    return Some(Err(ParseError::new(
                        format!(
                            "Expected record key, but found {}",
                            t.token.to_string().light_red()
                        ),
                        start_info.join(&t.info),
                    )
                    .with_label("This is not a valid record key".to_string(), t.info)));
                }
            };
            if let Err(err) = self.parse_expected_eq(TokenKind::Colon, ":") {
                return Some(Err(err));
            }
            let value = match self.parse_expr(COMMA_PREC) {
                Ok(value) => value,
                Err(err) => return Some(Err(err)),
            };
            fields.push((key, value));
            if let Ok(t) = self.lexer.next_token() {
                match t.token {
                    TokenKind::Op(op) if op == COMMA_SYM => continue,
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
                            start_info.join(&t.info),
                        )
                        .with_label(
                            "This should be either a comma or a right brace".to_string(),
                            t.info,
                        )));
                    }
                }
            }
        }
        Some(Ok(Ast::Record {
            fields,
            info: start_info.join(&last_info),
        }))
    }

    fn parse_primary(&mut self) -> ParseResult {
        let t = self
            .lexer
            .expect_next_token_not(pred::ignore)
            .map_err(|err| {
                ParseError::new(
                    "Expected primary expression".to_string(),
                    err.info().clone(),
                )
                .with_label(err.message().to_owned(), err.info().clone())
            })?;
        log::trace!("Parsing primary: {:?}", t.token);
        match t.token {
            lit if lit.is_literal() => self.parse_literal(&lit, t.info),
            TokenKind::Identifier(id) => Ok(Ast::Identifier {
                name: id,
                info: t.info,
            }),
            TokenKind::Op(op) => {
                if let Some(op) = self.find_operator_pos(&op, OpPos::Prefix) {
                    log::trace!("Parsing prefix operator: {:?}", op);
                    Ok(Ast::Unary {
                        op_info: op.clone(),
                        expr: Box::new(self.parse_term()?),
                        info: t.info,
                    })
                } else {
                    Err(ParseError::new(
                        format!("Expected prefix operator, but found {}", op.light_red()),
                        t.info.clone(),
                    )
                    .with_label("This is not a valid prefix operator".to_string(), t.info))
                }
            }

            start if start.is_grouping_start() => {
                match start {
                    TokenKind::LeftParen {
                        is_function_call: false,
                    } => self.parse_tuple(), // Tuples, Units, and Parentheses: ()
                    TokenKind::LeftBrace => self.parse_record_or_block(t.info), // Records and Blocks: {}
                    TokenKind::LeftBracket => self.parse_list(t.info),          // Lists: []
                    _ => unreachable!(),
                }
            }
            _ => Err(ParseError::new(
                format!(
                    "Expected primary expression, but found {}",
                    t.token.to_string().light_red()
                ),
                t.info.clone(),
            )
            .with_label(
                format!("The {} is invalid here", t.token.to_string().yellow()),
                t.info,
            )),
        }
    }

    fn parse_term(&mut self) -> ParseResult {
        let primary = self.parse_primary()?;
        // Check if function call with parentheses like `f(5, 6, 7)`, **NOT** `f (5, 6, 7)`
        if let Ok(nt) = self.lexer.peek_token(0) {
            if matches!(
                &nt.token,
                TokenKind::LeftParen {
                    is_function_call: true
                }
            ) {
                self.lexer.next_token().unwrap();
                let args = match self.parse_tuple()? {
                    Ast::Tuple { exprs, .. } => exprs,
                    single_expr => vec![single_expr],
                };
                return Ok(utils::roll_function_call(primary, args));
            }
        }
        Ok(primary)
    }

    /// Check if to continue parsing the next expression in the sequence
    /// based on the precedence of the next operator.
    ///
    /// ## Returns
    /// - `Some(op)`: If the next token is an infix binary operator that either:
    ///     - Has a precedence **greater than** `min_prec`.
    ///     - Is **right-associative** with a precedence **greater than or equal** to `min_prec`.
    ///     - `allow_eq` is `true` and precedence **equal** to `min_prec`.
    /// - `None`: If the next token is either:
    ///     - **Not an infix operator**.
    ///     - Its **precedence is lower than** `min_prec`.
    ///     - It is a **terminator**.
    fn check_binary_op(&self, min_prec: OpPrec, op: &str) -> Option<OpInfo> {
        let op = self.find_operator(op, |op| op.position == OpPos::Infix)?;
        let is_greater = op.precedence > min_prec;
        let is_right_assoc = op.associativity == OpAssoc::Right;
        let is_equal = op.precedence == min_prec;
        if is_greater || (is_right_assoc && is_equal) {
            Some(op.clone())
        } else {
            None
        }
    }

    fn check_postfix_op(&self, min_prec: OpPrec, op: &str) -> Option<OpInfo> {
        let op = self.find_operator(op, |op| op.position == OpPos::Postfix)?;
        let is_greater = op.precedence > min_prec;
        let is_right_assoc = op.associativity == OpAssoc::Right;
        let is_equal = op.precedence == min_prec;
        if is_greater || (is_right_assoc && is_equal) {
            Some(op.clone())
        } else {
            None
        }
    }

    /// Parse an expression with a given left-hand side and minimum precedence level
    /// using the operator precedence parsing (Pratt parsing) algorithm.
    ///
    /// ## Arguments
    /// - `lhs` The left-hand side of the expression
    /// - `min_prec` The minimum precedence of the expression
    ///
    /// ## Returns
    /// The parsed expression or a parse error if the expression could not be parsed
    ///
    /// ## Algorithm
    /// See: https://matklad.github.io/2020/04/13/simple-but-powerful-pratt-parsing.html
    fn parse_expr(&mut self, min_prec: OpPrec) -> ParseResult {
        let mut expr = self.parse_term()?;
        // println!("Parsed term: {:?}", expr);
        while let Ok(nt) = self.lexer.peek_token(0) {
            if nt.token.is_terminator() {
                break; // Stop parsing on expression terminators
            }
            if let TokenKind::Op(op) = &nt.token {
                if let Some(op) = self.check_postfix_op(min_prec, op) {
                    log::trace!("Parsing postfix operator: {:?}", op);
                    self.lexer.next_token().unwrap();
                    expr = Ast::Unary {
                        info: expr.info().join(&nt.info),
                        op_info: op.clone(),
                        expr: Box::new(expr),
                    };
                    continue;
                } else if let Some(op) = self.check_binary_op(min_prec, op) {
                    log::trace!("Parsing infix operator: {:?}", op);
                    self.lexer.next_token().unwrap();
                    let rhs = self.parse_expr(op.precedence)?;

                    let info = expr.info().join(rhs.info());
                    expr = match op.symbol.as_str() {
                        ASSIGNMENT_SYM => {
                            // Allow all definitions in the parser, even if they are not valid in the current context
                            utils::assignment(expr, rhs, info)?
                        }
                        MEMBER_ACCESS_SYM => utils::member_access(expr, rhs, info)?,
                        _ => Ast::Binary {
                            lhs: Box::new(expr),
                            op_info: op.clone(),
                            rhs: Box::new(rhs),
                            info,
                        },
                    };
                    continue;
                } else {
                    break;
                }
            }
            if FUNCTION_APP_PREC > min_prec {
                let call_info = expr.info().join(&nt.info);
                // Allow all definitions in the parser, even if they are not valid in the current context
                expr = utils::call(expr, self.parse_term()?, call_info)?;
                continue;
            }
            if nt.token.is_terminator() {
                break; // Stop parsing on expression terminators
            } else {
                return Err(ParseError::new(
                    format!(
                        "Expected operator or funciton application, but found {}",
                        nt.token.to_string().light_red()
                    ),
                    nt.info.clone(),
                )
                .with_label("Not valid in this context".to_string(), nt.info));
            }
        }
        Ok(expr)
    }

    /// Parse a top-level expression.
    fn parse_top_expr(&mut self) -> ParseResult {
        match self.parse_expr(0) {
            Ok(expr) => {
                self.skip_terminal_and_ignored();
                // Allow all definitions in the parser, even if they are not valid in the current context
                utils::top(expr)
            }
            Err(err) => Err(err),
        }
    }

    /// Skip all ignored tokens and the next top-level terminal token (`;`, `\n`, `EOF`).
    fn skip_terminal_and_ignored(&mut self) {
        // Remove all ignored tokens after the expression
        while let Ok(t) = self.lexer.peek_token(0) {
            if pred::ignore(&t.token) {
                self.lexer.next_token().unwrap();
            } else {
                break;
            }
        }
        // If the next token is a top-level terminal, consume it
        if let Ok(t) = self.lexer.peek_token(0) {
            if t.token.is_top_level_terminal(false) {
                self.lexer.next_token().unwrap();
            }
        }
        // Continue to ignore any remaining ignored tokens
        while let Ok(t) = self.lexer.peek_token(0) {
            if pred::ignore(&t.token) {
                self.lexer.next_token().unwrap();
            } else {
                break;
            }
        }
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
        if let Ok(t) = self.lexer.peek_token_not(pred::ignore, 0) {
            if pred::eof(&t.token) {
                return Ok(Ast::Literal {
                    value: Value::Unit,
                    info: t.info,
                });
            }
        }
        self.parse_top_expr()
    }

    /// Parse a global AST from the stream of tokens.
    /// A global AST is a list of **all** top-level AST nodes (expressions).
    pub fn parse_all(&mut self) -> ParseResults {
        let mut asts = Vec::new();
        loop {
            if let Ok(t) = self.lexer.peek_token_not(pred::ignore, 0) {
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
}
