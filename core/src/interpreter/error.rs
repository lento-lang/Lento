use crate::util::error::{BaseError, LineInfo};

/// Runtime error
#[derive(Debug, Clone)]
pub struct RuntimeError {
    pub inner: BaseError,
}

impl RuntimeError {
    pub fn new(message: String, info: LineInfo) -> Self {
        Self {
            inner: BaseError::new(message, info),
        }
    }
}
