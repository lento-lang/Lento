use crate::type_checker::types::Type;

use super::{lexer::InputSource, token::LineInfo};

#[derive(Debug, Clone)]
pub struct LexerError {
    pub message: String,
    pub info: LineInfo,
    pub input_source: InputSource,
}

impl LexerError {
    pub fn new(message: String, info: LineInfo, input_source: InputSource) -> Self {
        Self {
            message,
            info,
            input_source,
        }
    }

    pub fn unexpected_end_of_file(info: LineInfo, input_source: InputSource) -> Self {
        Self::new(
            format!("Unexpected end of {}", input_source),
            info,
            input_source,
        )
    }

    pub fn unexpected_character(c: char, info: LineInfo, input_source: InputSource) -> Self {
        Self::new(format!("Unexpected character '{}'", c), info, input_source)
    }

    pub fn invalid_char(c: String, info: LineInfo, input_source: InputSource) -> Self {
        Self::new(
            format!("Invalid character literal '{}'", c),
            info,
            input_source,
        )
    }

    pub fn invalid_number_type(
        num: &str,
        ty: &Type,
        info: LineInfo,
        input_source: InputSource,
    ) -> Self {
        Self::new(
            format!("Invalid {} literal '{}'", ty, num),
            info,
            input_source,
        )
    }

    pub fn invalid_number(num: &str, info: LineInfo, input_source: InputSource) -> Self {
        Self::new(
            format!("Invalid number literal '{}'", num),
            info,
            input_source,
        )
    }
}
