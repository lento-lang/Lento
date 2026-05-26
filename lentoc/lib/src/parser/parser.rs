use super::{
    ast::Ast,
    error::{ParseError, ParserOpError},
    op::{
        prec::{COMMA_PREC, FUNCTION_APP_PREC},
        OpAssoc, OpInfo, OpPos, OpPrec,
    },
};
use crate::type_checker::checked_ast::TypeAst;
use crate::{
    interpreter::value::{RecordKey, Value},
    lexer::{
        lexer::{self, Lexer},
        readers::{bytes_reader::BytesReader, stdin::StdinReader},
        token::{Keyword, Token, TokenInfo},
    },
    parser::{op::prec, pattern::BindPattern},
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
    use crate::lexer::token::Token;

    pub fn eof(t: &Token) -> bool {
        matches!(t, Token::EndOfFile)
    }

    /// Check if the token is an ignore token.
    /// These include:
    /// - `Newline`
    /// - `Comment`
    pub fn ignore(t: &Token) -> bool {
        matches!(t, Token::Comment(_) | Token::Newline)
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
pub(crate) const FN_ARROW_SYM: &str = "->";
pub(crate) const EFFECT_ASCRIPTION_SYM: &str = "!";
pub(crate) const SUM_TYPE_SYM: &str = "|";

/// Default operators used in the language grammar and required for parsing. \
/// These operators are defined in the parser and are required to produce valid ASTs. \
/// The binary operators are replaced with `Ast` nodes by `syntax_sugar::specialize` after parsing a `parse_top` expression.
/// - `semicolon`: `;` - Used to separate statements becomes an `Ast::Block` node.
/// - `comma`: `,` - Used to separate expressions in tuples and lists becomes an `Ast::Tuple` or `Ast::List` node.
/// - `assignment`: `=` - Used to assign values to variables becomes an `Ast::Let` node.
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
        // Effect annotation operator (e.g. `A -> B ! { e }`)
        OpInfo {
            symbol: EFFECT_ASCRIPTION_SYM.to_string(),
            position: OpPos::Infix,
            precedence: prec::EFFECT_ASCRIPTION_PREC,
            associativity: OpAssoc::Left,
            allow_trailing: false,
        },
        // Effect annotation as prefix (e.g. session types `!t . s`)
        OpInfo {
            symbol: "!".to_string(),
            position: OpPos::Prefix,
            precedence: prec::PREFIX_PREC,
            associativity: OpAssoc::Right,
            allow_trailing: false,
        },
        // Function arrow (e.g. `A -> B`)
        OpInfo {
            symbol: FN_ARROW_SYM.to_string(),
            position: OpPos::Infix,
            precedence: prec::FUNCTION_ARROW_PREC,
            associativity: OpAssoc::Right,
            allow_trailing: false,
        },
        // Sum types (e.g. `int | str`)
        OpInfo {
            symbol: SUM_TYPE_SYM.to_string(),
            position: OpPos::Infix,
            precedence: prec::SUM_TYPE_PREC,
            associativity: OpAssoc::Left,
            allow_trailing: false,
        },
        // Intersection / constraint types
        OpInfo {
            symbol: "&".to_string(),
            position: OpPos::Infix,
            precedence: prec::LOGICAL_AND_PREC,
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
        condition: impl FnOnce(&Token) -> bool,
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
        expected_token: Token,
        symbol: &'static str,
    ) -> Result<TokenInfo, ParseError> {
        self.parse_expected(|t| t == &expected_token, symbol)
    }

    // ======================================== EXPRESSION PARSING ======================================== //

    /// Parse a literal `Value` from the lexer.
    fn parse_literal(&mut self, token: &Token, info: LineInfo) -> ParseResult {
        Ok(Ast::Literal {
            value: match token {
                Token::Number(n) => Value::Number(n.clone()),
                Token::String(s) => Value::String(s.clone()),
                Token::Char(c) => Value::Char(*c),
                Token::Boolean(b) => Value::Boolean(*b),
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
            if t.token == Token::RightParen {
                self.lexer.next_token().unwrap();
                return Ok(Ast::unit(t.info));
            }
        }
        log::trace!("Parsing elements...");
        let tuple = self.parse_top()?;
        log::trace!("Parsed tuple elements: {:?}", tuple);
        self.parse_expected_eq(Token::RightParen, ")")?;
        Ok(tuple)
    }

    fn parse_record_or_block(&mut self, start_info: LineInfo) -> ParseResult {
        // Try to parse as record
        if let Some(res) = self.parse_record_fields(&start_info) {
            res
        } else {
            self.parse_block(start_info)
        }
    }

    fn parse_block(&mut self, start_info: LineInfo) -> Result<Ast, ParseError> {
        // Parse as block
        let mut exprs = Vec::new();
        while let Ok(end) = self.lexer.peek_token(0) {
            if end.token == Token::RightBrace {
                break;
            }
            exprs.push(self.parse_top()?);
        }
        let last = self.parse_expected_eq(Token::RightBrace, "}")?;
        Ok(Ast::Block {
            exprs,
            info: start_info.join(&last.info),
        })
    }

    fn parse_list(&mut self, start_info: LineInfo) -> ParseResult {
        let mut exprs = Vec::new();
        while let Ok(end) = self.lexer.peek_token(0) {
            if end.token == Token::RightBracket {
                break;
            }
            exprs.push(self.parse_expr(COMMA_PREC)?);
            if let Ok(nt) = self.lexer.peek_token(0) {
                if nt.token == Token::Operator(COMMA_SYM.to_string()) {
                    self.lexer.next_token().unwrap();
                    continue;
                } else if nt.token == Token::RightBracket {
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
        let last = self.parse_expected_eq(Token::RightBracket, "]")?;
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
                Token::RightBrace => {
                    self.lexer.next_token().unwrap();
                    return Some(Ok(Ast::Record {
                        fields,
                        info: t.info,
                    })); // Empty record
                }
                Token::Identifier(id) => RecordKey::String(id),
                // TokenKind::Number(n) => RecordKey::Number(n),
                Token::String(s) => RecordKey::String(s),
                Token::Char(c) => RecordKey::String(c.to_string()),
                _ => return None, // Not a record
            };
            let Ok(t) = self.lexer.peek_token(1) else {
                return None;
            };
            if t.token != Token::Colon {
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
                    Token::Operator(op) if op == COMMA_SYM => {
                        last_info = t.info;
                    }
                    Token::RightBrace => {
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
            if t.token == Token::RightBrace {
                last_info = t.info;
                break;
            }
            let key = match t.token {
                Token::Identifier(id) => RecordKey::String(id),
                // TokenKind::Number(n) => RecordKey::Number(n),
                Token::String(s) => RecordKey::String(s),
                Token::Char(c) => RecordKey::String(c.to_string()),
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
            if let Err(err) = self.parse_expected_eq(Token::Colon, ":") {
                return Some(Err(err));
            }
            let value = match self.parse_expr(COMMA_PREC) {
                Ok(value) => value,
                Err(err) => return Some(Err(err)),
            };
            fields.push((key, value));
            if let Ok(t) = self.lexer.next_token() {
                match t.token {
                    Token::Operator(op) if op == COMMA_SYM => continue,
                    Token::RightBrace => {
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
            Token::Identifier(id) => Ok(Ast::Identifier {
                name: id,
                info: t.info,
            }),
            Token::Keyword(ref kw) => match kw {
                Keyword::Self_ => Ok(Ast::Identifier {
                    name: "Self".to_string(),
                    info: t.info,
                }),
                Keyword::Intrinsic => Ok(Ast::Identifier {
                    name: "intrinsic".to_string(),
                    info: t.info,
                }),
                _ => Err(ParseError::new(
                    format!("Unexpected keyword: {}", t.token.to_string().light_red()),
                    t.info.clone(),
                )
                .with_label(
                    format!(
                        "The keyword {} is not valid here",
                        t.token.to_string().yellow()
                    ),
                    t.info,
                )),
            },
            Token::Operator(op) => {
                if let Some(op) = self.find_operator_pos(&op, OpPos::Prefix) {
                    log::trace!("Parsing prefix operator: {:?}", op);
                    Ok(Ast::Unary {
                        op: op.clone(),
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
                    Token::LeftParen {
                        is_function_call: false,
                    } => self.parse_tuple(), // Tuples, Units, and Parentheses: ()
                    Token::LeftBrace => self.parse_record_or_block(t.info), // Records and Blocks: {}
                    Token::LeftBracket => self.parse_list(t.info),          // Lists: []
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
                Token::LeftParen {
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
            let is_top_level_nl_term = min_prec == 0 && nt.token.is_top_level_terminal(true);
            if nt.token.is_terminator() || is_top_level_nl_term {
                break; // Stop parsing on expression terminators
            }
            if let Token::Operator(op) = &nt.token {
                if let Some(op) = self.check_postfix_op(min_prec, op) {
                    log::trace!("Parsing postfix operator: {:?}", op);
                    self.lexer.next_token().unwrap();
                    expr = Ast::Unary {
                        info: expr.info().join(&nt.info),
                        op: op.clone(),
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
                            log::debug!(
                                "Specializing assignment: {} = {}",
                                expr.print_expr(),
                                rhs.print_expr()
                            );
                            log::trace!("Specializing assignment: {:?} = {:?}", expr, rhs);

                            // Try to parse other generic binding patterns (non-typed) for assignments like:
                            // `_ = ...`, `x = ...`, `[x, y] = ...`, `{ a: x, b: y } = ...`, etc.
                            Ast::Let {
                                target: BindPattern::from_expr(expr)?,
                                expr: Box::new(rhs),
                                annotation: None,
                                info,
                            }
                        }
                        MEMBER_ACCESS_SYM => utils::member_access(expr, rhs, info)?,
                        // COMMA_SYM => utils::into_tuple(expr, rhs, info)?,
                        _ => Ast::Binary {
                            lhs: Box::new(expr),
                            op: op.clone(),
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
                // expr = utils::call(expr, self.parse_term()?, call_info)?;
                let arg = self.parse_term()?;
                expr = Ast::FunctionCall {
                    expr: Box::new(expr),
                    arg: Box::new(arg),
                    info: call_info,
                };
                continue;
            }
            if nt.token.is_terminator() || is_top_level_nl_term {
                break; // Stop parsing on expression terminators
            } else {
                return Err(ParseError::new(
                    format!(
                        "Expected operator or function application, but found {}",
                        nt.token.to_string().light_red()
                    ),
                    nt.info.clone(),
                )
                .with_label("Not valid in this context".to_string(), nt.info));
            }
        }
        log::trace!("Completed expression (pre): {:?}", expr);
        let expr = utils::expr_top(expr)?;
        log::trace!("Completed expression (post): {:?}", expr);
        Ok(expr)
    }

    fn parse_statement(&mut self) -> Result<Option<Ast>, ParseError> {
        let Ok(t) = self.lexer.peek_token_not(pred::ignore, 0) else {
            return Ok(None);
        };
        if t.token.is_keyword(&Keyword::Let) {
            self.lexer.next_token().unwrap(); // consume let
            return self.parse_let_stmt().map(Some);
        }
        if t.token.is_keyword(&Keyword::Fn) {
            self.lexer.next_token().unwrap(); // consume fn
            return self.parse_fn().map(Some);
        }
        if t.token.is_keyword(&Keyword::Type) {
            self.lexer.next_token().unwrap(); // consume type
            return self.parse_type_decl().map(Some);
        }
        Ok(None)
    }

    /// Parse a `let` statement after the `let` keyword has been consumed.
    fn parse_let_stmt(&mut self) -> ParseResult {
        let expr = self.parse_expr(0)?;
        match expr {
            Ast::Let { .. } => Ok(expr),
            _ => Err(ParseError::new(
                "Expected `let <pattern> = <expr>`".to_string(),
                expr.info().clone(),
            )),
        }
    }

    /// Parse a top-level expression.
    fn parse_top(&mut self) -> ParseResult {
        if let Some(stmt) = self.parse_statement()? {
            self.skip_terminal_and_ignored();
            return Ok(stmt);
        }
        match self.parse_expr(0) {
            Ok(expr) => {
                self.skip_terminal_and_ignored();
                Ok(expr)
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

    /// Parse a function declaration or definition.
    ///
    /// Expects the `fn` keyword to already be consumed.
    ///
    /// ## Declaration syntax
    /// ```lento
    /// fn name :: return_type ! { effects }
    /// ```
    ///
    /// ## Definition syntax
    /// ```lento
    /// fn name(param1, param2, ...) -> return_type ! { effects } = body
    /// fn name(param1, param2, ...) -> return_type ! { effects } { body }
    /// ```
    fn parse_fn(&mut self) -> ParseResult {
        // Parse function name
        let name_t = self
            .lexer
            .expect_next_token_not(pred::ignore)
            .map_err(|err| {
                ParseError::new(
                    "Expected function name after fn".to_string(),
                    err.info().clone(),
                )
            })?;
        let name = match &name_t.token {
            Token::Identifier(id) => id.clone(),
            _ => {
                return Err(ParseError::new(
                    format!(
                        "Expected function name, but found {}",
                        name_t.token.to_string().light_red()
                    ),
                    name_t.info.clone(),
                )
                .with_label("This should be a function name".to_string(), name_t.info));
            }
        };
        let name_info = name_t.info;

        // Peek to distinguish declaration (::) from definition ((...))
        let next = self.lexer.peek_token_not(pred::ignore, 0).map_err(|err| {
            ParseError::new(
                "Expected :: or ( after function name".to_string(),
                err.info().clone(),
            )
        })?;

        match &next.token {
            Token::DoubleColon => self.parse_fn_declaration(name, name_info),
            Token::LeftParen { .. } => self.parse_fn_definition(name, name_info),
            _ => Err(ParseError::new(
                format!(
                    "Expected :: or ( after function name, but found {}",
                    next.token.to_string().light_red()
                ),
                next.info.clone(),
            )
            .with_label(
                "Function declarations use `::`; definitions use `(`".to_string(),
                next.info,
            )),
        }
    }

    fn parse_fn_declaration(&mut self, name: String, name_info: LineInfo) -> ParseResult {
        self.lexer.next_token().unwrap(); // consume ::
        let sig_expr = self.parse_top()?;
        let sig_info = sig_expr.info().clone();
        let signature =
            crate::type_checker::specialize::into_type_ast(sig_expr.clone()).map_err(|e| {
                ParseError::new(
                    format!("Expected type signature after `::`: {}", e.message()),
                    sig_expr.info().clone(),
                )
            })?;
        Ok(Ast::FunctionDecl {
            name,
            signature,
            info: name_info.join(&sig_info),
        })
    }

    fn parse_fn_definition(&mut self, name: String, name_info: LineInfo) -> ParseResult {
        use crate::lexer::token::Keyword;

        self.lexer.next_token().unwrap(); // consume (
        let mut params: Vec<(BindPattern, Option<TypeAst>)> = Vec::new();
        loop {
            // Check for immediate )
            if let Ok(t) = self.lexer.peek_token_not(pred::ignore, 0) {
                if t.token == Token::RightParen {
                    self.lexer.next_token().unwrap();
                    break;
                }
            }
            let (pattern, param_type) = if let Ok(next) = self.lexer.peek_token_not(pred::ignore, 0)
            {
                if let Token::Identifier(name) = &next.token {
                    let name = name.clone();
                    let name_info = next.info.clone();
                    self.lexer.next_token().unwrap();
                    let mut parsed_ty: Option<TypeAst> = None;
                    if let Ok(colon) = self.lexer.peek_token_not(pred::ignore, 0) {
                        if matches!(colon.token, Token::Colon) {
                            self.lexer.next_token().unwrap(); // consume ':'
                            let annotation = self.parse_expr(prec::COMMA_PREC + 1)?;
                            parsed_ty =
                                Some(crate::type_checker::specialize::into_type_ast(annotation)?);
                        }
                    }
                    (
                        BindPattern::Variable {
                            name,
                            info: name_info,
                        },
                        parsed_ty,
                    )
                } else {
                    // Parse parameter as an expression (tuple, record, list, etc.)
                    // Use a min prec above comma to stop at parameter boundaries
                    let param_expr = self.parse_expr(prec::COMMA_PREC + 1)?;
                    (
                        BindPattern::from_expr(param_expr).map_err(|e| {
                            ParseError::new(
                                format!("Invalid function parameter: {}", e.message()),
                                e.info().clone(),
                            )
                        })?,
                        None,
                    )
                }
            } else {
                return Err(ParseError::new(
                    "Expected function parameter".to_string(),
                    name_info.clone(),
                ));
            };
            params.push((pattern, param_type));
            // Check for comma
            if let Ok(t) = self.lexer.peek_token_not(pred::ignore, 0) {
                if t.token == Token::Operator(COMMA_SYM.to_string()) {
                    self.lexer.next_token().unwrap();
                    continue;
                }
            }
        }

        // Parse optional -> return type expression (may include ! effects)
        // Use a min prec just above assignment so `= body` isn't consumed.
        let mut return_type_expr: Option<Ast> = None;
        if let Ok(t) = self.lexer.peek_token_not(pred::ignore, 0) {
            if matches!(&t.token, Token::Operator(op) if op == FN_ARROW_SYM) {
                self.lexer.next_token().unwrap(); // consume ->
                return_type_expr = Some(self.parse_expr(prec::FUNCTION_APP_PREC + 1)?);
            }
        }

        // Parse optional requires { ... } / ensures { ... }
        let mut requires: Option<Ast> = None;
        let mut ensures: Option<Ast> = None;
        loop {
            if let Ok(t) = self.lexer.peek_token_not(pred::ignore, 0) {
                if t.token.is_keyword(&Keyword::Requires) {
                    self.lexer.next_token().unwrap();
                    self.parse_expected_eq(Token::LeftBrace, "{")?;
                    requires = Some(self.parse_top()?);
                    self.parse_expected_eq(Token::RightBrace, "}")?;
                    continue;
                }
                if t.token.is_keyword(&Keyword::Ensures) {
                    self.lexer.next_token().unwrap();
                    self.parse_expected_eq(Token::LeftBrace, "{")?;
                    ensures = Some(self.parse_top()?);
                    self.parse_expected_eq(Token::RightBrace, "}")?;
                    continue;
                }
            }
            break;
        }

        // Parse body
        let body: Ast;
        if let Ok(t) = self.lexer.peek_token_not(pred::ignore, 0) {
            if t.token == Token::Operator(ASSIGNMENT_SYM.to_string()) {
                // Expression body: fn name(params) -> ... = expr
                self.lexer.next_token().unwrap(); // consume =
                body = self.parse_top()?;
            } else if t.token == Token::LeftBrace {
                // Block body: fn name(params) -> ... { body }
                self.lexer.next_token().unwrap(); // consume {
                body = self.parse_block(t.info)?;
            } else {
                return Err(ParseError::new(
                    format!(
                        "Expected `=` or `{{` after function signature, found {}",
                        t.token
                    ),
                    t.info,
                ));
            }
        } else {
            return Err(ParseError::new(
                "Expected `=` or `{` after function signature".to_string(),
                LineInfo::default(),
            ));
        }

        let body_info = body.info().clone();
        Ok(Ast::FunctionDef {
            name,
            params,
            return_type: return_type_expr.map(Box::new),
            requires: requires.map(Box::new),
            ensures: ensures.map(Box::new),
            body: Box::new(body),
            info: name_info.join(&body_info),
        })
    }

    /// Parse a type declaration after the `type` keyword has been consumed.
    ///
    /// ## Syntax
    /// ```lento
    /// type Name = <type-expr>
    /// type Name(Params) = <type-expr>
    /// ```
    fn parse_type_decl(&mut self) -> ParseResult {
        let name_token = self.lexer.next_token().map_err(|_| {
            ParseError::new(
                "Expected type name after `type`".to_string(),
                LineInfo::default(),
            )
        })?;
        let name = match &name_token.token {
            Token::Identifier(name) => name.clone(),
            _ => {
                return Err(ParseError::new(
                    format!("Expected type name, found {}", name_token.token),
                    name_token.info,
                ))
            }
        };
        let name_info = name_token.info;

        // Check for optional parenthesized parameter list: type Name(Param1, Param2)
        let params: Vec<Ast> = if let Ok(t) = self.lexer.peek_token_not(pred::ignore, 0) {
            if matches!(t.token, Token::LeftParen { .. }) {
                self.lexer.next_token().unwrap(); // consume (
                let mut p = Vec::new();
                loop {
                    let param_name = self.lexer.next_token().map_err(|_| {
                        ParseError::new("Expected parameter name".to_string(), LineInfo::default())
                    })?;
                    match &param_name.token {
                        Token::Identifier(name) => {
                            p.push(Ast::Identifier {
                                name: name.clone(),
                                info: param_name.info.clone(),
                            });
                        }
                        _ => {
                            return Err(ParseError::new(
                                format!("Expected parameter name, found {}", param_name.token),
                                param_name.info.clone(),
                            ))
                        }
                    }
                    if let Ok(t) = self.lexer.peek_token_not(pred::ignore, 0) {
                        if t.token == Token::RightParen {
                            self.lexer.next_token().unwrap(); // consume )
                            break;
                        } else if matches!(&t.token, Token::Operator(s) if s == ",") {
                            self.lexer.next_token().unwrap(); // consume ,
                            continue;
                        } else {
                            return Err(ParseError::new(
                                format!("Expected `,` or `)`, found {}", t.token),
                                t.info,
                            ));
                        }
                    } else {
                        return Err(ParseError::new(
                            "Expected `)` after type parameters".to_string(),
                            param_name.info,
                        ));
                    }
                }
                p
            } else {
                vec![]
            }
        } else {
            vec![]
        };

        // Expect `=`
        if let Ok(t) = self.lexer.peek_token_not(pred::ignore, 0) {
            if t.token != Token::Operator("=".to_string()) {
                return Err(ParseError::new(
                    format!("Expected `=` after type name, found {}", t.token),
                    t.info,
                ));
            }
            self.lexer.next_token().unwrap(); // consume =
        } else {
            return Err(ParseError::new(
                "Expected `=` after type name".to_string(),
                name_info.clone(),
            ));
        }

        // Parse the body type expression
        let body_expr = self.parse_expr(0)?;
        let body_info = body_expr.info().clone();
        let body =
            crate::type_checker::specialize::into_type_ast(body_expr.clone()).map_err(|e| {
                ParseError::new(
                    format!(
                        "Expected type expression in type declaration: {}",
                        e.message()
                    ),
                    body_expr.info().clone(),
                )
            })?;

        Ok(Ast::TypeDecl {
            name,
            params,
            body,
            info: name_info.join(&body_info),
        })
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
        self.parse_top()
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
            match self.parse_top() {
                Ok(expr) => asts.push(expr),
                Err(e) => return Err(e),
            }
        }
        Ok(asts)
    }
}

mod utils {
    use super::*;
    use crate::type_checker::types::std_types;

    pub fn flatten_sequence(expr: Ast, binary_op_symbol: &str) -> Vec<Ast> {
        let mut exprs = Vec::new();
        let mut queue = vec![expr];
        while let Some(current) = queue.pop() {
            match current {
                // Flatten sequences of expressions
                Ast::Binary {
                    lhs,
                    op: op_info,
                    rhs,
                    ..
                } if op_info.symbol == binary_op_symbol => {
                    queue.push(*rhs);
                    queue.push(*lhs);
                }
                _ => exprs.push(current),
            }
        }
        exprs
    }

    /// Takes a function name, a list of arguments and rolls them into a single function call expression.
    /// Arguments are rolled into a nested function call.
    /// All arguments are sorted like:
    /// ```lento
    /// func(a, b, c)
    /// ```
    /// becomes:
    /// ```lento
    /// func(a)(b)(c)
    /// ```
    pub fn roll_function_call(expr: Ast, args: Vec<Ast>) -> Ast {
        let last_info = args
            .last()
            .map(|a| a.info().clone())
            .unwrap_or(expr.info().clone());
        let call_info = expr.info().join(&last_info);

        // If the expression is not a type constructor, we can create a function call as is.
        log::trace!(
            "Creating function call: {}({})",
            expr.print_expr().light_blue(),
            args.iter()
                .map(|a| a.print_expr().light_blue().to_string())
                .collect::<Vec<String>>()
                .join(", ")
        );
        let mut args = args.into_iter();
        let mut call = Ast::FunctionCall {
            expr: Box::new(expr),
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

    pub fn member_access(expr: Ast, rhs: Ast, info: LineInfo) -> ParseResult {
        log::trace!(
            "Specializing member access: {}.{}",
            expr.print_expr().light_blue(),
            rhs.print_expr().light_blue()
        );
        Ok(Ast::MemberAccess {
            expr: Box::new(expr),
            field: record_key(rhs)?,
            info,
        })
    }

    pub fn expr_top(expr: Ast) -> ParseResult {
        match expr {
            // Specialize a comma-separated sequence of expressions into a tuple.
            // E.g:
            // - `x, y, z` becomes `(x, y, z)`
            Ast::Binary { lhs, op, rhs, info } if op.symbol == COMMA_SYM => {
                log::trace!("Specializing comma sequence: {:?}, {:?}", lhs, rhs);
                let mut exprs = flatten_sequence(*lhs, COMMA_SYM);
                exprs.push(*rhs);
                Ok(Ast::Tuple {
                    info: info.join(exprs.last().unwrap().info()),
                    exprs,
                })
            }
            _ => Ok(expr),
        }
    }

    pub fn record_key(expr: Ast) -> Result<RecordKey, ParseError> {
        match expr {
            Ast::Identifier { name, .. } => Ok(RecordKey::String(name.to_string())),
            // Ast::Literal {
            //     value: Value::Number(Number::UnsignedInteger(n)),
            //     ..
            // } => Ok(RecordKey::Number(Number::UnsignedInteger(n.clone()))),
            _ => Err(ParseError::new(
                format!(
                    "Field access via {} requires an identifier or {} literal",
                    ".".yellow(),
                    std_types::UINT().pretty_print_color()
                ),
                expr.info().clone(),
            )
            .with_label(
                format!(
                    "This is not an identifier or {}",
                    std_types::UINT().pretty_print_color()
                ),
                expr.info().clone(),
            )
            .with_hint(format!(
                "Did you mean to use indexing via {} instead?",
                "[]".yellow()
            ))),
        }
    }
}
