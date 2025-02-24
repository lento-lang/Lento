use crate::type_checker::types::Type;

use super::token::LineInfo;

#[derive(Debug, Clone)]
pub struct LexerError {
    pub message: String,
    pub info: LineInfo,
}

impl LexerError {
    pub fn new(message: String, info: LineInfo) -> Self {
        Self { message, info }
    }

    pub fn unexpected_end_of_file(info: LineInfo) -> Self {
        Self::new("Unexpected end of program".to_string(), info)
    }

    pub fn unexpected_character(c: char, info: LineInfo) -> Self {
        Self::new(format!("Unexpected character '{}'", c), info)
    }

    pub fn invalid_char(c: String, info: LineInfo) -> Self {
        Self::new(format!("Invalid character literal '{}'", c), info)
    }

    pub fn invalid_number_type(num: &str, ty: &Type, info: LineInfo) -> Self {
        Self::new(format!("Invalid {} literal '{}'", ty, num), info)
    }

    pub fn invalid_number(num: &str, info: LineInfo) -> Self {
        Self::new(format!("Invalid number literal '{}'", num), info)
    }
}
