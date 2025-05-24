use crate::{
    interpreter::value::{RecordKey, Value},
    type_checker::types::TypeJudgements,
    util::error::LineInfo,
};

use super::ast::TypeAst;

/// A pattern used for binding variables in:
/// - Variable assignments
/// - Function definitions
/// - Function arguments
/// - Destructuring assignments
#[derive(Debug, Clone)]
pub enum BindPattern {
    /// A variable binding pattern.
    Variable {
        /// The name of the variable.
        name: String,
        /// The type annotation for the variable.
        info: LineInfo,
    },
    /// A function definition binding pattern.
    Function {
        /// The name of the function.
        name: String,
        /// Parameters of the function.
        params: Vec<BindPattern>,
        /// Information about the function.
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
        value: Value,
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
            BindPattern::Function { info, .. } => info,
            BindPattern::Tuple { info, .. } => info,
            BindPattern::Record { info, .. } => info,
            BindPattern::List { info, .. } => info,
            BindPattern::Wildcard => panic!("Wildcard pattern has no line info"),
            BindPattern::Literal { info, .. } => info,
            BindPattern::Rest { info, .. } => info,
        }
    }

    pub fn specialize(&mut self, _judgements: &TypeJudgements, _changed: &mut bool) {
        match self {
            BindPattern::Variable { .. } => (),
            BindPattern::Function { params, .. } => {
                for param in params {
                    param.specialize(_judgements, _changed);
                }
            }
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
            BindPattern::Function {
                name,
                params,
                ..
            } => {
                let mut result = format!("{}(", name);
                for (i, v) in params.iter().enumerate() {
                    result.push_str(&v.print_expr());
                    if i < params.len() - 1 {
                        result.push_str(", ");
                    }
                }
                result.push(')');
                result
            }
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
            BindPattern::Literal { value, .. } => value.pretty_print(),
            BindPattern::Rest { name, .. } => format!("...{}", name),
        }
    }

    pub fn pretty_print(&self) -> String {
        match self {
            BindPattern::Variable { name, .. } => name.clone(),
            BindPattern::Function {
                name,
                params,
                ..
            } => {
                let mut result = format!("{}(", name);
                for (i, v) in params.iter().enumerate() {
                    result.push_str(&v.pretty_print());
                    if i < params.len() - 1 {
                        result.push_str(", ");
                    }
                }
                result.push(')');
                result
            }
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
            BindPattern::Literal { value, .. } => value.pretty_print(),
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
