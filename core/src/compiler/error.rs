use crate::util::error::{BaseError, BaseErrorExt, LineInfo};

/// Lexer error
#[derive(Debug, Clone)]
pub struct CompileError {
    pub inner: BaseError,
}

impl BaseErrorExt for CompileError {
    fn new(message: String, info: LineInfo) -> Self {
        Self {
            inner: BaseError::new(message, info),
        }
    }

    fn with_hint(self, hint: String) -> Self {
        Self {
            inner: self.inner.with_hint(hint),
        }
    }

    fn with_label(self, message: String, info: LineInfo) -> Self {
        Self {
            inner: self.inner.with_label(message, info),
        }
    }

    fn base(&self) -> &BaseError {
        &self.inner
    }

    fn to_base(self) -> BaseError {
        self.inner
    }
}
