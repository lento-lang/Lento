use crate::lexer::token::LineInfo;

/// Lexer error
#[derive(Debug, Clone)]
pub struct CompileError {
    pub message: String,
    pub info: LineInfo,
}

impl CompileError {
    pub fn new(message: String, info: LineInfo) -> Self {
        Self { message, info }
    }
}
