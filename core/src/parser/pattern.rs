use super::{ast::Ast, error::ParseError};
use crate::{
    interpreter::{
        number::{Number, SignedInteger, UnsignedInteger},
        value::{RecordKey, Value},
    },
    type_checker::types::TypeJudgements,
    util::error::{BaseErrorExt, LineInfo},
};
use std::hash::Hash;

/// A pattern used for binding variables in:
/// - Variable assignments
/// - Function definitions
/// - Function arguments
/// - Destructuring assignments
#[derive(Debug, Clone, Eq)]
pub enum BindPattern {
    /// A variable binding pattern.
    Variable {
        /// The name of the variable.
        name: String,
        /// The type annotation for the variable.
        info: LineInfo,
    },
    /// A tuple binding pattern.
    Tuple {
        /// The elements of the tuple.
        elements: Vec<BindPattern>,
        info: LineInfo,
    },
    /// A record binding pattern.
    Record {
        /// The fields of the record.
        fields: Vec<(RecordKey, BindPattern)>,
        info: LineInfo,
    },
    /// A list binding pattern.
    List {
        /// The elements of the list.
        elements: Vec<BindPattern>,
        info: LineInfo,
    },
    /// A wildcard pattern that matches any value.
    Wildcard,
    /// A literal pattern that matches a specific value.
    Literal {
        /// The value to match.
        value: LiteralPattern,
        info: LineInfo,
    },
    /// A rest of a collection pattern that matches the rest of a list.
    Rest {
        /// The name of the variable to bind the rest of the list.
        name: String,
        /// The type annotation for the variable.
        info: LineInfo,
    },
}

impl BindPattern {
    pub fn info(&self) -> &LineInfo {
        match self {
            BindPattern::Variable { info, .. } => info,
            BindPattern::Tuple { info, .. } => info,
            BindPattern::Record { info, .. } => info,
            BindPattern::List { info, .. } => info,
            BindPattern::Wildcard => panic!("Wildcard pattern has no line info"),
            BindPattern::Literal { info, .. } => info,
            BindPattern::Rest { info, .. } => info,
        }
    }

    // Convert a loose AST expression into a binding pattern.
    pub fn from_expr(expr: Ast) -> Result<Self, ParseError> {
        match expr {
            Ast::Identifier { name, .. } if name.starts_with("_") => Ok(BindPattern::Wildcard),
            Ast::Identifier { name, info } => Ok(BindPattern::Variable { name, info }),
            Ast::Tuple { exprs, info } => {
                let elements = exprs
                    .into_iter()
                    .map(BindPattern::from_expr)
                    .collect::<Result<Vec<_>, _>>()?;
                Ok(BindPattern::Tuple { elements, info })
            }
            Ast::Record { fields, info } => {
                let fields = fields
                    .into_iter()
                    .map(|(k, v)| Ok((k, BindPattern::from_expr(v)?)))
                    .collect::<Result<Vec<_>, _>>()?;
                Ok(BindPattern::Record { fields, info })
            }
            Ast::List { exprs, info } => {
                let elements = exprs
                    .into_iter()
                    .map(BindPattern::from_expr)
                    .collect::<Result<Vec<_>, _>>()?;
                Ok(BindPattern::List { elements, info })
            }
            Ast::Literal { value, info } => Ok(BindPattern::Literal {
                value: LiteralPattern::from_value(value).ok_or(
                    ParseError::new("Expected a literal value pattern".to_string(), info.clone())
                        .with_label("This is not valid".to_string(), info.clone()),
                )?,
                info,
            }),
            _ => Err(ParseError::new(
                format!("Invalid binding pattern: {}", expr.print_expr()),
                expr.info().clone(),
            )),
        }
    }

    pub fn specialize(&mut self, _judgements: &TypeJudgements, _changed: &mut bool) {
        match self {
            BindPattern::Variable { .. } => (),
            BindPattern::Tuple { elements, .. } => {
                for element in elements {
                    element.specialize(_judgements, _changed);
                }
            }
            BindPattern::Record { fields, .. } => {
                for (_, element) in fields {
                    element.specialize(_judgements, _changed);
                }
            }
            BindPattern::List { elements, .. } => {
                for element in elements {
                    element.specialize(_judgements, _changed);
                }
            }
            BindPattern::Wildcard => (),
            BindPattern::Literal { .. } => (),
            BindPattern::Rest { .. } => (),
        }
    }

    pub fn print_expr(&self) -> String {
        match self {
            BindPattern::Variable { name, .. } => name.clone(),

            BindPattern::Tuple { elements, .. } => format!(
                "({})",
                elements
                    .iter()
                    .map(|e| e.print_expr())
                    .collect::<Vec<String>>()
                    .join(", ")
            ),
            BindPattern::Record { fields, .. } => format!(
                "{{ {} }}",
                fields
                    .iter()
                    .map(|(k, v)| format!("{}: {}", k, v.print_expr()))
                    .collect::<Vec<String>>()
                    .join(", ")
            ),
            BindPattern::List { elements, .. } => format!(
                "[{}]",
                elements
                    .iter()
                    .map(|e| e.print_expr())
                    .collect::<Vec<String>>()
                    .join(", ")
            ),
            BindPattern::Wildcard => "_".to_string(),
            BindPattern::Literal { value, .. } => value.as_value().pretty_print(),
            BindPattern::Rest { name, .. } => format!("...{}", name),
        }
    }

    pub fn pretty_print(&self) -> String {
        match self {
            BindPattern::Variable { name, .. } => name.clone(),
            BindPattern::Tuple { elements, .. } => {
                let mut result = "(".to_string();
                for (i, v) in elements.iter().enumerate() {
                    result.push_str(&v.pretty_print());
                    if i < elements.len() - 1 {
                        result.push_str(", ");
                    }
                }
                result.push(')');
                result
            }
            BindPattern::Record { fields, .. } => {
                let mut result = "{ ".to_string();
                for (i, (k, v)) in fields.iter().enumerate() {
                    result.push_str(&format!("{}: {}", k, v.pretty_print()));
                    if i < fields.len() - 1 {
                        result.push_str(", ");
                    }
                }
                result.push_str(" }");
                result
            }
            BindPattern::List { elements, .. } => {
                let mut result = "[".to_string();
                for (i, v) in elements.iter().enumerate() {
                    result.push_str(&v.pretty_print());
                    if i < elements.len() - 1 {
                        result.push_str(", ");
                    }
                }
                result.push(']');
                result
            }
            BindPattern::Wildcard => "_".to_string(),
            BindPattern::Literal { value, .. } => value.as_value().pretty_print(),
            BindPattern::Rest { name, .. } => format!("...{}", name),
        }
    }

    pub fn is_wildcard(&self) -> bool {
        matches!(self, BindPattern::Wildcard)
    }
}

impl PartialEq for BindPattern {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Variable { name: l0, .. }, Self::Variable { name: r0, .. }) => l0 == r0,
            (Self::Tuple { elements: l0, .. }, Self::Tuple { elements: r0, .. }) => l0 == r0,
            (Self::Record { fields: l0, .. }, Self::Record { fields: r0, .. }) => l0 == r0,
            (Self::List { elements: l0, .. }, Self::List { elements: r0, .. }) => l0 == r0,
            (Self::Wildcard, Self::Wildcard) => true,
            (Self::Literal { value: l0, .. }, Self::Literal { value: r0, .. }) => l0 == r0,
            (Self::Rest { name: l0, .. }, Self::Rest { name: r0, .. }) => l0 == r0,
            _ => false,
        }
    }
}

impl Hash for BindPattern {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        match self {
            BindPattern::Variable { name, .. } => name.hash(state),
            BindPattern::Tuple { elements, .. } => {
                for element in elements {
                    element.hash(state);
                }
            }
            BindPattern::Record { fields, .. } => {
                for (key, value) in fields {
                    key.hash(state);
                    value.hash(state);
                }
            }
            BindPattern::List { elements, .. } => {
                for element in elements {
                    element.hash(state);
                }
            }
            BindPattern::Wildcard => "Wildcard".hash(state),
            BindPattern::Literal { value, .. } => value.hash(state),
            BindPattern::Rest { name, .. } => name.hash(state),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum LiteralPattern {
    UnsignedInteger(UnsignedInteger),
    SignedInteger(SignedInteger),
    String(String),
    Char(char),
    Boolean(bool),
}

impl LiteralPattern {
    /// A helper function to convert a `Value` into a `LiteralPattern`.
    pub fn from_value(value: Value) -> Option<Self> {
        Some(match value {
            Value::Number(n) => match n {
                Number::UnsignedInteger(u) => LiteralPattern::UnsignedInteger(u),
                Number::SignedInteger(i) => LiteralPattern::SignedInteger(i),
                _ => return None, // Only unsigned and signed integers are supported as literals
            },
            Value::String(s) => LiteralPattern::String(s),
            Value::Char(c) => LiteralPattern::Char(c),
            Value::Boolean(b) => LiteralPattern::Boolean(b),
            _ => return None,
        })
    }

    pub fn into_value(self) -> Value {
        match self {
            LiteralPattern::UnsignedInteger(value) => Value::Number(Number::UnsignedInteger(value)),
            LiteralPattern::SignedInteger(value) => Value::Number(Number::SignedInteger(value)),
            LiteralPattern::String(value) => Value::String(value),
            LiteralPattern::Char(value) => Value::Char(value),
            LiteralPattern::Boolean(value) => Value::Boolean(value),
        }
    }

    pub fn as_value(&self) -> Value {
        match self {
            LiteralPattern::UnsignedInteger(value) => {
                Value::Number(Number::UnsignedInteger(value.clone()))
            }
            LiteralPattern::SignedInteger(value) => {
                Value::Number(Number::SignedInteger(value.clone()))
            }
            LiteralPattern::String(value) => Value::String(value.clone()),
            LiteralPattern::Char(value) => Value::Char(*value),
            LiteralPattern::Boolean(value) => Value::Boolean(*value),
        }
    }
}
