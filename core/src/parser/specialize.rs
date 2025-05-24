use super::{
    ast::{Ast, TypeAst},
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
    util::error::{BaseErrorExt, LineInfo},
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
            target: binding_pattern_top(*lhs, types)?,
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
            lhs,
            op_info,
            rhs,
            info,
        } if op_info.symbol == COMMA_SYM => {
            let mut exprs = flatten_sequence(*lhs, *rhs);
            if exprs.len() == 1 {
                Ok(exprs.pop().unwrap())
            } else {
                log::trace!("Specializing comma sequence: {:?}", exprs);
                Ok(Ast::Tuple {
                    info: info.join(exprs.last().unwrap().info()),
                    exprs,
                })
            }
        }
        // No specialization available
        _ => Ok(expr),
    }
}

// Specialize type constructors to literal types
pub fn call(expr: Ast, types: &HashSet<String>) -> ParseResult {
    let mut exprs = flatten_calls(&expr);
    let func = exprs.remove(0);
    if let Ast::Identifier { name, info: _ } = &func {
        if types.contains(name) {
            todo!("Specialize type constructor");
        }
    }
    Ok(expr)
}

fn flatten_calls(expr: &Ast) -> Vec<&Ast> {
    let mut exprs = Vec::new();
    let mut current = expr;
    while let Ast::FunctionCall { expr, arg, .. } = current {
        exprs.push(&**arg);
        current = expr;
    }
    exprs.push(current);
    exprs.reverse();
    exprs
}

/// Flatten a sequence of comma (bin op) separated expressions into a single vector
fn flatten_sequence(expr: Ast, first: Ast) -> Vec<Ast> {
    let mut exprs = vec![first];
    let mut current = expr;
    loop {
        match current {
            Ast::Binary {
                lhs, op_info, rhs, ..
            } if op_info.symbol == COMMA_SYM => {
                exprs.push(*rhs);
                current = *lhs;
            }
            _ => {
                exprs.push(current);
                break;
            }
        }
    }
    exprs.reverse();
    exprs
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
fn binding_pattern_top(expr: Ast, types: &HashSet<String>) -> Result<BindPattern, ParseError> {
    match expr {
        // `_ = ...`
        Ast::Identifier { name, .. } if name.starts_with("_") => Ok(BindPattern::Wildcard),
        // `x = ...`
        Ast::Identifier { name, info } => Ok(BindPattern::Variable {
            name,
            annotation: None,
            info,
        }),
        // `x, y = ...`, `f x, y = ...`, `f int x, bool y = ...` or `int f int x, bool y = ...` etc.
        // Potentially a function definition.
        Ast::Binary {
            lhs, op_info, rhs, ..
        } if op_info.symbol == COMMA_SYM => {
            let exprs = flatten_sequence(*lhs, *rhs);
            comma_function_definition(exprs)
        }
        // `int x = ...`, `f x = ...`, `f int x = ...`, `int f x = ...` or `int f int x = ...`.
        // Potentially a function definition.
        Ast::FunctionCall { ref info, .. } => {
            let mut exprs = flatten_calls(&expr);
            match try_type_annotation(&exprs, types) {
                Some(Ok((func_annotation, mut exprs))) => {
                    // Found a function definition with a type annotation!
                    let func = exprs.remove(0);
                    if let Ast::Identifier { name, .. } = func {
                        if exprs.is_empty() {
                            // `int x = ...`
                            // If no parameters are found, it's a typed variable binding
                            Ok(BindPattern::Variable {
                                name: name.to_string(),
                                annotation: Some(func_annotation),
                                info: expr.info().clone(),
                            })
                        } else {
                            function_definition(Some(func_annotation), name, exprs, info)
                        }
                    } else {
                        Err(ParseError::new(
                            format!("Invalid function binding pattern: {}", expr.print_expr()),
                            expr.info().clone(),
                        ))
                    }
                }
                None => {
                    // If no type annotation is found, we assume it's a function definition without type annotations.
                    let func = exprs.remove(0);
                    if let Ast::Identifier { name, .. } = func {
                        function_definition(None, name, exprs, info)
                    } else {
                        Err(ParseError::new(
                            format!("Invalid function binding pattern: {}", expr.print_expr()),
                            expr.info().clone(),
                        ))
                    }
                }
                Some(Err(err)) => Err(err),
            }
        }
        _ => Err(ParseError::new(
            format!("Invalid binding pattern: {}", expr.print_expr()),
            expr.info().clone(),
        )),
    }
}

fn function_definition(
    annotation: Option<TypeAst>,
    name: &str,
    rest: Vec<&Ast>,
    start_info: &LineInfo,
) -> Result<BindPattern, ParseError> {
    let mut params = Vec::new();
    for expr in rest {
        match expr {
            Ast::Identifier { name, info } => {
                params.push(BindPattern::Variable {
                    name: name.to_string(),
                    annotation: None,
                    info: info.clone(),
                });
            }
            _ => {
                return Err(ParseError::new(
                    format!("Invalid function binding pattern: {}", expr.print_expr()),
                    expr.info().clone(),
                ));
            }
        }
    }
    let last_info = params
        .last()
        .map_or(start_info.clone(), |p| p.info().clone());
    Ok(BindPattern::Function {
        name: name.to_string(),
        annotation,
        params,
        info: start_info.join(&last_info),
    })
}

// // Convert a loose AST expression into a binding pattern.
// fn binding_pattern(expr: &Ast) -> Result<BindPattern, ParseError> {
//     match expr {
//         Ast::Identifier { name, .. } if name.starts_with("_") => Ok(BindPattern::Wildcard),
//         Ast::Identifier { name, info } => Ok(BindPattern::Variable {
//             name,
//             annotation: None,
//             info,
//         }),
//         Ast::Tuple { exprs, info } => {
//             let elements = exprs
//                 .into_iter()
//                 .map(binding_pattern)
//                 .collect::<Result<Vec<_>, _>>()?;
//             Ok(BindPattern::Tuple { elements, info })
//         }
//         Ast::Record { fields, info } => {
//             let fields = fields
//                 .into_iter()
//                 .map(|(k, v)| Ok((k, binding_pattern(v)?)))
//                 .collect::<Result<Vec<_>, _>>()?;
//             Ok(BindPattern::Record { fields, info })
//         }
//         Ast::List { exprs, info } => {
//             let elements = exprs
//                 .into_iter()
//                 .map(binding_pattern)
//                 .collect::<Result<Vec<_>, _>>()?;
//             Ok(BindPattern::List { elements, info })
//         }
//         Ast::Literal { value, info } => Ok(BindPattern::Literal { value, info }),
//         _ if matches!(expr, Ast::FunctionCall { .. }) => {
//             log::trace!(
//                 "Specializing function binding pattern: {}",
//                 expr.print_expr()
//             );
//             let mut args = flatten_calls(&expr);
//             let func = args.remove(0);
//             if let Ast::Identifier { name, info } = func {
//                 let params = args
//                     .into_iter()
//                     .map(|arg| binding_pattern((*arg).clone()))
//                     .collect::<Result<Vec<_>, _>>()?;
//                 Ok(BindPattern::Function {
//                     name: name.clone(),
//                     annotation: None,
//                     params,
//                     info: info.clone(),
//                 })
//             } else {
//                 Err(ParseError::new(
//                     format!("Invalid function binding pattern: {}", expr.print_expr()),
//                     expr.info().clone(),
//                 ))
//             }
//         }
//         _ => Err(ParseError::new(
//             format!("Invalid binding pattern: {}", expr.print_expr()),
//             expr.info().clone(),
//         )),
//     }
// }

fn comma_function_definition(exprs: Vec<Ast>) -> Result<BindPattern, ParseError> {
    let flat = exprs.iter().map(flatten_calls).collect::<Vec<_>>();
    log::trace!(
        "Specializing comma separated assignment pattern: [{}]",
        flat.iter()
            .map(|e| {
                format!(
                    "[{}]",
                    e.iter()
                        .map(|e| e.print_expr())
                        .collect::<Vec<_>>()
                        .join(", ")
                )
            })
            .collect::<Vec<_>>()
            .join(", ")
    );
    todo!("Specialize comma separated function definition");
}

/// Try to parse a type annotation from a list of expressions given from a function definition assignment.
///
/// ## Example
/// ```ignored
/// int f str x, bool y
/// ```
/// Becomes: (`int`, `f(str(x)), bool(y)`)
///
/// ## Example
/// ```ignored
/// List int f int x
/// ```
/// Becomes: (`List(int)`, `f(int(x))`)
fn try_type_annotation<'a>(
    exprs: &'a [&'a Ast],
    types: &HashSet<String>,
) -> Option<Result<(TypeAst, Vec<&'a Ast>), ParseError>> {
    let mut annotations = Vec::new();
    let mut rest = Vec::new();
    let mut is_annotation = true;
    for expr in exprs {
        if is_annotation {
            match &expr {
                // `int f str x, bool y` or `int f int x`
                Ast::Identifier { name, .. } if types.contains(name) => {
                    annotations.push(expr);
                }
                // `(((HashMap int) str) f) int x`
                // We need to unwrap the type constructor call
                // until we reach a non-type identifier
                Ast::FunctionCall {
                    expr: func,
                    arg,
                    info,
                } => {
                    let mut exprs = flatten_calls(func);
                }
                _ => {
                    is_annotation = false;
                    rest.push(expr);
                }
            }
        } else {
            rest.push(expr);
        }
    }
    None
    // Create a type from the annotation expressoins
    // Some(Ok((TypeAst::Constructor {
    //     expr: types,
    //     params,
    // })))
}
