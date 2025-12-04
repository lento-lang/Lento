use std::fmt::Debug;

use crate::util::error::{BaseError, BaseErrorExt, LineInfo};

#[derive(Debug, Clone, PartialEq)]
pub struct ParseError {
    inner: BaseError,
}

impl BaseErrorExt for ParseError {
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

/// Errors for when an operator is inserted into the parser
/// operator table.
#[derive(Debug, PartialEq)]
pub enum ParserOpError {
    AlreadyExists,
}

/// Errors for when a type is inserted into the parser
/// type table.
#[derive(Debug, PartialEq)]
pub enum ParseTypeError {
    TypeExists,
    NonLiteralType,
}
