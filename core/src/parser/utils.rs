use crate::{
    interpreter::{
        number::Number,
        value::{RecordKey, Value},
    },
    parser::{
        ast::Ast,
        error::ParseError,
        parser::ParseResult,
        pattern::{BindPattern, LiteralPattern},
    },
    type_checker::types::std_types,
    util::error::{BaseErrorExt, LineInfo},
};
use colorful::Colorful;

/// Convert a loose AST expression into a binding pattern.
pub fn assignment(target: Ast, body: Ast, assignment_info: LineInfo) -> ParseResult {
    log::debug!(
        "Specializing assignment: {} = {}",
        target.print_expr(),
        body.print_expr()
    );
    log::trace!("Specializing assignment: {:?} = {:?}", target, body);

    // Try parse other generic binding patterns (non-typed) for assignments like:
    // `_ = ...`, `x = ...`, `[x, y] = ...`, `{ a: x, b: y } = ...`, etc.
    Ok(Ast::Assignment {
        annotation: None,
        target: binding_pattern(target)?,
        expr: Box::new(body),
        info: assignment_info,
    })
}

pub fn member_access(expr: Ast, rhs: Ast, info: LineInfo) -> ParseResult {
    log::trace!(
        "Specializing member access: {}.{}",
        expr.print_expr().light_blue(),
        rhs.print_expr().light_blue()
    );
    Ok(Ast::MemberAccess {
        expr: Box::new(expr),
        field: record_key(rhs)?,
        info,
    })
}
pub fn record_key(expr: Ast) -> Result<RecordKey, ParseError> {
    match expr {
        Ast::Identifier { name, .. } => Ok(RecordKey::String(name.to_string())),
        // Ast::Literal {
        //     value: Value::Number(Number::UnsignedInteger(n)),
        //     ..
        // } => Ok(RecordKey::Number(Number::UnsignedInteger(n.clone()))),
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

// Specialize type constructors to literal types
pub fn call(expr: Ast, arg: Ast, info: LineInfo) -> ParseResult {
    Ok(match expr {
        Ast::LiteralType {
            expr:
                TypeAst::Constructor {
                    expr,
                    mut params,
                    info,
                },
        } if is_type_expr(&arg, types) => {
            // If the expression is a literal type constructor, we can add the argument as a type parameter.
            log::trace!(
                "Specializing existing type constructor: {}({}, {})",
                expr.print_expr().light_blue(),
                params
                    .iter()
                    .map(|p| p.pretty_print())
                    .collect::<Vec<String>>()
                    .join(", ")
                    .light_blue(),
                arg.print_expr().light_blue()
            );
            params.push(into_type_ast(arg)?);
            Ast::LiteralType {
                expr: TypeAst::Constructor { expr, params, info },
            }
        }
        Ast::FunctionCall {
            expr: inner,
            arg: arg_inner,
            info: inner_info,
        } => {
            match *arg_inner {
                Ast::Identifier {
                    name: constructor_name,
                    info: constructor_info,
                } if types.contains(&constructor_name) && is_type_expr(&arg, types) => {
                    // Apply the new type argument to the type constructor
                    log::trace!(
                        "Specializing new type constructor call: {}({})",
                        constructor_name.clone().light_blue(),
                        arg.print_expr().light_blue()
                    );
                    let args = vec![into_type_ast(arg)?];
                    Ast::FunctionCall {
                        expr: inner,
                        arg: Box::new(Ast::LiteralType {
                            expr: TypeAst::Constructor {
                                expr: Box::new(TypeAst::Identifier {
                                    name: constructor_name,
                                    info: constructor_info,
                                }),
                                params: args,
                                info: inner_info.join(&info),
                            },
                        }),
                        info: inner_info,
                    }
                }
                // If the expression is not a type constructor, create a function call as is.
                _ => Ast::FunctionCall {
                    expr: Box::new(Ast::FunctionCall {
                        expr: inner,
                        arg: arg_inner,
                        info: inner_info,
                    }),
                    arg: Box::new(arg),
                    info,
                },
            }
        }
        // If the expression is not a type constructor, create a function call as is.
        _ => Ast::FunctionCall {
            expr: Box::new(expr),
            arg: Box::new(arg),
            info,
        },
    })
}

/// Takes a function name, a list of arguments and rolls them into a single function call expression.
/// Arguments are rolled into a nested function call.
/// All arguments are sorted like:
/// ```lento
/// func(a, b, c)
/// ```
/// becomes:
/// ```lento
/// func(a)(b)(c)
/// ```
pub fn roll_function_call(expr: Ast, args: Vec<Ast>) -> Ast {
    let last_info = args
        .last()
        .map(|a| a.info().clone())
        .unwrap_or(expr.info().clone());
    let call_info = expr.info().join(&last_info);

    // If the expression is not a type constructor, we can create a function call as is.
    log::trace!(
        "Creating function call: {}({})",
        expr.print_expr().light_blue(),
        args.iter()
            .map(|a| a.print_expr().light_blue().to_string())
            .collect::<Vec<String>>()
            .join(", ")
    );
    let mut args = args.into_iter();
    let mut call = Ast::FunctionCall {
        expr: Box::new(expr),
        arg: Box::new(args.next().unwrap()),
        info: call_info.clone(),
    };
    for arg in args {
        let arg_info = call_info.join(arg.info());
        call = Ast::FunctionCall {
            expr: Box::new(call),
            arg: Box::new(arg),
            info: arg_info,
        };
    }
    call
}

// Convert a loose AST expression into a binding pattern.
pub fn binding_pattern(expr: Ast) -> Result<BindPattern, ParseError> {
    match expr {
        Ast::Identifier { name, .. } if name.starts_with("_") => Ok(BindPattern::Wildcard),
        Ast::Identifier { name, info } => Ok(BindPattern::Variable { name, info }),
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
        Ast::Literal { value, info } => Ok(BindPattern::Literal {
            value: literal_pattern(value).ok_or(
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

/// A helper function to convert a `Value` into a `LiteralPattern`.
pub fn literal_pattern(value: Value) -> Option<LiteralPattern> {
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
