use std::fmt::Debug;

use crate::util::error::{BaseError, LineInfo};

#[derive(Debug, Clone, PartialEq)]
pub struct ParseError {
    pub inner: BaseError,
}

impl ParseError {
    pub fn new(message: String, info: LineInfo) -> Self {
        Self {
            inner: BaseError::new(message, info),
        }
    }
}

/// Errors for when an operator is inserted into the parser
/// operator table.
#[derive(Debug, PartialEq)]
pub enum ParseOperatorError {
    PositionForSymbolExists,
    /// Any operator cannot override an existing static operator.
    CannotOverrideStaticOperator,
    /// When adding a static operator, no other operator with the same symbol can exist.
    NonStaticOperatorExists,
}

/// Errors for when a type is inserted into the parser
/// type table.
#[derive(Debug, PartialEq)]
pub enum ParseTypeError {
    TypeExists,
    NonLiteralType,
}
