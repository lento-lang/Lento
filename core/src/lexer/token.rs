use std::fmt::{Debug, Display};

use crate::{interpreter::number::Number, util::error::LineInfo};

// Token structure for the Lento programming language
#[derive(Debug, Clone, PartialEq)]
pub enum Token {
    EndOfFile,
    // Expression terminators
    Newline,
    SemiColon,
    Colon,
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
