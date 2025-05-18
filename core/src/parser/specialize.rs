use super::{
    ast::Ast,
    error::ParseError,
    op::{ASSIGNMENT_SYM, COMMA_SYM, MEMBER_ACCESS_SYM},
    parser::ParseResult,
};
use crate::{
    interpreter::{
        number::Number,
        value::{RecordKey, Value},
    },
    parser::pattern::BindPattern,
    type_checker::types::std_types,
    util::error::BaseErrorExt,
};
use colorful::Colorful;
use std::collections::HashSet;

//--------------------------------------------------------------------------------------//
//                                   Syntax Sugar                                       //
//--------------------------------------------------------------------------------------//

/// Specialize any top-level expressions by transforming them into
/// their concrete `AST` representation from loose `AST` representation.
/// This is done by transforming:
/// - `a; b; c` into `Block { exprs: [a, b, c] }`
/// - `x = y` into `Assignment { target: BindPatter::Variable { name: x }, expr: y }`
/// - `x.y` into `MemberAccess { expr: x, field: y }`
/// - `f(x, y) = b` into `Assignment { target: BindPattern::Function { name: f, params: [x, y] }, expr: b }`
/// - `f x, y = b` into `Assignment { target: BindPattern::Function { name: f, params: [x, y] }, expr: b }`
/// - `int f str x, bool y = b` into `Assignment { annotation: Some(int), target: BindPattern::Function { name: f, params: [str x, bool y] }, expr: b }`
/// - `List int` into `LiteralType { expr: TypeAst::Constructor { expr: List }, args: [int] }`
pub fn top(expr: Ast, types: &HashSet<String>) -> ParseResult {
    match expr {
        // Specialize type constructors to literal types
        Ast::FunctionCall { .. } => call(expr, types),
        // Specialize assignments to binding patterns with optional type annotations
        Ast::Binary {
            lhs,
            op_info,
            rhs,
            info,
        } if op_info.symbol == ASSIGNMENT_SYM => Ok(Ast::Assignment {
            annotation: None,
            target: binding_pattern(*lhs)?,
            expr: rhs,
            info,
        }),
        Ast::Binary {
            lhs,
            op_info,
            rhs,
            info,
        } if op_info.symbol == MEMBER_ACCESS_SYM => Ok(Ast::MemderAccess {
            expr: lhs,
            field: record_key(*rhs)?,
            info,
        }),
        Ast::Binary {
            lhs, op_info, rhs, ..
        } if op_info.symbol == COMMA_SYM => Ok(sequence(*lhs, *rhs)),
        // No specialization available
        _ => Ok(expr),
    }
}

// Specialize type constructors to literal types
pub fn call(expr: Ast, types: &HashSet<String>) -> ParseResult {
    let (func, _args) = flatten_calls(&expr);
    if let Ast::Identifier { name, info: _ } = &func {
        if types.contains(name) {
            todo!("Specialize type constructor");
        }
    }
    Ok(expr)
}

fn flatten_calls(expr: &Ast) -> (&Ast, Vec<&Ast>) {
    let mut calls = Vec::new();
    let mut current = expr;
    while let Ast::FunctionCall { expr, arg, .. } = current {
        calls.push(&**arg);
        current = expr;
    }
    (current, calls)
}

/// Flatten a sequence of comma (bin op) separated expressions into a single vector
fn sequence(expr: Ast, first: Ast) -> Ast {
    let mut exprs = vec![first];
    let start_info = expr.info().clone();
    let mut current = expr;
    loop {
        match current {
            Ast::Binary {
                lhs, op_info, rhs, ..
            } if op_info.symbol == COMMA_SYM => {
                exprs.insert(0, *rhs);
                current = *lhs;
            }
            _ => {
                exprs.insert(0, current);
                break;
            }
        }
    }
    if exprs.len() == 1 {
        exprs.pop().unwrap()
    } else {
        log::trace!("Specializing comma sequence: {:?}", exprs);
        Ast::Tuple {
            info: start_info.join(exprs.last().unwrap().info()),
            exprs,
        }
    }
}

fn record_key(expr: Ast) -> Result<RecordKey, ParseError> {
    match expr {
        Ast::Identifier { name, .. } => Ok(RecordKey::String(name.to_string())),
        Ast::Literal {
            value: Value::Number(Number::UnsignedInteger(n)),
            ..
        } => Ok(RecordKey::Number(Number::UnsignedInteger(n.clone()))),
        _ => Err(ParseError::new(
            format!(
                "Field access via {} requires a identifier or {} literal",
                ".".yellow(),
                std_types::UINT().pretty_print_color()
            ),
            expr.info().clone(),
        )
        .with_label(
            format!(
                "This is not an identifier or {}",
                std_types::UINT().pretty_print_color()
            ),
            expr.info().clone(),
        )
        .with_hint(format!(
            "Did you mean to use indexing via {} instead?",
            "[]".yellow()
        ))),
    }
}

/// Convert a loose AST expression into a binding pattern.
fn binding_pattern(expr: Ast) -> Result<BindPattern, ParseError> {
    match expr {
        Ast::Identifier { name, info } => Ok(BindPattern::Variable {
            name,
            annotation: None,
            info,
        }),
        Ast::Tuple { exprs, info } => {
            let elements = exprs
                .into_iter()
                .map(binding_pattern)
                .collect::<Result<Vec<_>, _>>()?;
            Ok(BindPattern::Tuple { elements, info })
        }
        Ast::Record { fields, info } => {
            let fields = fields
                .into_iter()
                .map(|(k, v)| Ok((k, binding_pattern(v)?)))
                .collect::<Result<Vec<_>, _>>()?;
            Ok(BindPattern::Record { fields, info })
        }
        Ast::List { exprs, info } => {
            let elements = exprs
                .into_iter()
                .map(binding_pattern)
                .collect::<Result<Vec<_>, _>>()?;
            Ok(BindPattern::List { elements, info })
        }
        Ast::Literal { value, info } => Ok(BindPattern::Literal { value, info }),
        _ => Err(ParseError::new(
            format!("Invalid binding pattern: {}", expr.print_expr()),
            expr.info().clone(),
        )),
    }
}
