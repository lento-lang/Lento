use crate::lexer::token::LineInfo;

/// Runtime error
#[derive(Debug, Clone)]
pub struct RuntimeError {
    pub message: String,
    pub info: LineInfo,
}

impl RuntimeError {
    pub fn new(message: String, info: LineInfo) -> Self {
        Self { message, info }
    }
}
