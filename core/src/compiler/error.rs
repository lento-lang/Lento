use crate::util::error::{BaseError, LineInfo};

/// Lexer error
#[derive(Debug, Clone)]
pub struct CompileError {
    pub inner: BaseError,
}

impl CompileError {
    pub fn new(message: String, info: LineInfo) -> Self {
        Self {
            inner: BaseError::new(message, info),
        }
    }
}
