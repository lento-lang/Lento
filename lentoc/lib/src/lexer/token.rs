use std::fmt::{Debug, Display};

use crate::{interpreter::number::Number, util::error::LineInfo};

/// Language keywords
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Keyword {
    Fn,
    Let,
    Type,
    Use,
    As,
    All,
    Effect,
    Handle,
    With,
    Infix,
    Ensures,
    Requires,
    Self_,
    In,
    Intrinsic,
    Exists,
    Match,
    If,
    Else,
    For,
    While,
    Is,
    End,
}

impl Keyword {
    pub fn all() -> &'static [&'static str] {
        &[
            "fn",
            "let",
            "type",
            "use",
            "as",
            "all",
            "effect",
            "handle",
            "with",
            "infix",
            "ensures",
            "requires",
            "Self",
            "in",
            "intrinsic",
            "exists",
            "match",
            "if",
            "else",
            "for",
            "while",
            "is",
            "end",
        ]
    }

    pub fn from_str(s: &str) -> Option<Keyword> {
        match s {
            "fn" => Some(Keyword::Fn),
            "let" => Some(Keyword::Let),
            "type" => Some(Keyword::Type),
            "use" => Some(Keyword::Use),
            "as" => Some(Keyword::As),
            "all" => Some(Keyword::All),
            "effect" => Some(Keyword::Effect),
            "handle" => Some(Keyword::Handle),
            "with" => Some(Keyword::With),
            "infix" => Some(Keyword::Infix),
            "ensures" => Some(Keyword::Ensures),
            "requires" => Some(Keyword::Requires),
            "Self" => Some(Keyword::Self_),
            "in" => Some(Keyword::In),
            "intrinsic" => Some(Keyword::Intrinsic),
            "exists" => Some(Keyword::Exists),
            "match" => Some(Keyword::Match),
            "if" => Some(Keyword::If),
            "else" => Some(Keyword::Else),
            "for" => Some(Keyword::For),
            "while" => Some(Keyword::While),
            "is" => Some(Keyword::Is),
            "end" => Some(Keyword::End),
            _ => None,
        }
    }
}

impl Display for Keyword {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Keyword::Fn => write!(f, "fn"),
            Keyword::Let => write!(f, "let"),
            Keyword::Type => write!(f, "type"),
            Keyword::Use => write!(f, "use"),
            Keyword::As => write!(f, "as"),
            Keyword::All => write!(f, "all"),
            Keyword::Effect => write!(f, "effect"),
            Keyword::Handle => write!(f, "handle"),
            Keyword::With => write!(f, "with"),
            Keyword::Infix => write!(f, "infix"),
            Keyword::Ensures => write!(f, "ensures"),
            Keyword::Requires => write!(f, "requires"),
            Keyword::Self_ => write!(f, "Self"),
            Keyword::In => write!(f, "in"),
            Keyword::Intrinsic => write!(f, "intrinsic"),
            Keyword::Exists => write!(f, "exists"),
            Keyword::Match => write!(f, "match"),
            Keyword::If => write!(f, "if"),
            Keyword::Else => write!(f, "else"),
            Keyword::For => write!(f, "for"),
            Keyword::While => write!(f, "while"),
            Keyword::Is => write!(f, "is"),
            Keyword::End => write!(f, "end"),
        }
    }
}

// Token structure for the Lento programming language
#[derive(Debug, Clone, PartialEq)]
pub enum Token {
    EndOfFile,
    // Expression terminators
    Newline,
    SemiColon,
    Colon,
    DoubleColon,
    // A language keyword
    Keyword(Keyword),
    // Literals
    Identifier(String),
    Number(Number),
    String(String),
    Char(char),
    Boolean(bool),
    // Grouping and separation tokens
    // (
    LeftParen {
        // If the left parenthesis is part of a function call
        is_function_call: bool,
    },
    RightParen,   // )
    LeftBrace,    // {
    RightBrace,   // }
    LeftBracket,  // [
    RightBracket, // ]
    // All other operators will be implemented in a standard library at runtime in the future
    // leaving support for user-defined operators
    Operator(String),
    // Comments
    Comment(String),
}

impl Token {
    pub fn is_literal(&self) -> bool {
        matches!(
            self,
            Token::Number(_) | Token::String(_) | Token::Char(_) | Token::Boolean(_)
        )
    }

    pub fn is_identifier(&self) -> bool {
        matches!(self, Token::Identifier(_))
    }

    pub fn is_keyword(&self) -> bool {
        matches!(self, Token::Keyword(_))
    }

    pub fn is_opening_keyword(&self) -> bool {
        matches!(
            self,
            Token::Keyword(Keyword::Fn)
                | Token::Keyword(Keyword::Let)
                | Token::Keyword(Keyword::Type)
                | Token::Keyword(Keyword::Use)
                | Token::Keyword(Keyword::Effect)
                | Token::Keyword(Keyword::Handle)
                | Token::Keyword(Keyword::With)
                | Token::Keyword(Keyword::Infix)
                | Token::Keyword(Keyword::Ensures)
                | Token::Keyword(Keyword::Requires)
                | Token::Keyword(Keyword::Match)
                | Token::Keyword(Keyword::If)
                | Token::Keyword(Keyword::Else)
                | Token::Keyword(Keyword::For)
                | Token::Keyword(Keyword::While)
        )
    }

    pub fn eq_keyword(&self, kw: &Keyword) -> bool {
        matches!(self, Token::Keyword(k) if k == kw)
    }

    pub fn is_terminator(&self) -> bool {
        matches!(
            self,
            Token::EndOfFile
                | Token::SemiColon
                | Token::RightParen
                | Token::RightBrace
                | Token::RightBracket
                | Token::Comment(_)
        )
    }

    pub fn is_newline(&self) -> bool {
        matches!(self, Token::Newline)
    }

    pub fn is_grouping_start(&self) -> bool {
        matches!(
            self,
            Token::LeftParen { .. } | Token::LeftBrace | Token::LeftBracket
        )
    }

    pub fn is_grouping_end(&self) -> bool {
        matches!(
            self,
            Token::RightParen | Token::RightBrace | Token::RightBracket
        )
    }

    pub fn is_grouping(&self) -> bool {
        self.is_grouping_start() || self.is_grouping_end()
    }

    pub fn is_top_level_terminal(&self, allow_eof: bool) -> bool {
        if allow_eof {
            matches!(self, Token::EndOfFile | Token::Newline | Token::SemiColon)
        } else {
            matches!(self, Token::Newline | Token::SemiColon)
        }
    }
}

impl Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::EndOfFile => write!(f, "end of program"),
            Self::Newline => write!(f, "newline"),
            Self::SemiColon => write!(f, ";"),
            Self::Colon => write!(f, ":"),
            Self::DoubleColon => write!(f, "::"),
            Self::Keyword(kw) => write!(f, "{}", kw),
            Self::Identifier(s) => write!(f, "{}", s),
            Self::Number(s) => write!(f, "{}", s),
            Self::String(s) => write!(f, "\"{}\"", s),
            Self::Char(c) => write!(f, "'{}'", c),
            Self::Boolean(b) => write!(f, "{}", b),
            Self::LeftParen { .. } => write!(f, "("),
            Self::RightParen => write!(f, ")"),
            Self::LeftBrace => write!(f, "{{"),
            Self::RightBrace => write!(f, "}}"),
            Self::LeftBracket => write!(f, "["),
            Self::RightBracket => write!(f, "]"),
            Self::Operator(s) => write!(f, "{}", s),
            Self::Comment(s) => write!(f, "// {}", s),
        }
    }
}

/// TokenInfo is a structure that contains a token and its line and column information
/// along with the character before and after the token.
/// This is used for error reporting and debugging.
#[derive(Clone)]
pub struct TokenInfo {
    /// The token itself
    pub token: Token,
    /// The line and column of the token
    pub info: LineInfo,
}

impl Debug for TokenInfo {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?} at {:?}", self.token, self.info)
    }
}
