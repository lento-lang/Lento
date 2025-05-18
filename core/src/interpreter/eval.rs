use std::borrow::Borrow;

use crate::{
    interpreter::value::NativeFunction,
    parser::pattern::BindPattern,
    type_checker::{
        checked_ast::CheckedAst,
        types::{GetType, Type},
    },
    util::{
        error::{BaseErrorExt, LineInfo},
        failable::Failable,
        str::Str,
    },
};

use super::{
    env::Environment,
    error::RuntimeError,
    value::{Function, UserFunction, Value},
};

//--------------------------------------------------------------------------------------//
//                                     Interpreter                                      //
//--------------------------------------------------------------------------------------//

pub type InterpretResult = Result<Value, RuntimeError>;

/// Interpret a module
pub fn eval_exprs(exprs: &[CheckedAst], env: &mut Environment) -> InterpretResult {
    let mut result = Value::Unit;
    for expr in exprs {
        result = eval_expr(expr, env)?;
    }
    Ok(result)
}

/// Interpret a type-checked AST node
pub fn eval_expr(ast: &CheckedAst, env: &mut Environment) -> InterpretResult {
    let result = match ast {
        CheckedAst::FunctionCall {
            expr: function,
            arg,
            ..
        } => eval_call(function, arg, env, ast.info())?,
        CheckedAst::Tuple { exprs, .. } => eval_tuple(exprs, env)?,
        CheckedAst::Literal { value, .. } => value.clone(),
        CheckedAst::LiteralType { value, .. } => Value::Type(value.clone()),
        CheckedAst::Identifier { name, .. } => match env.lookup_identifier(name) {
            (Some(v), _) => v.clone(),
            (_, Some(f)) => Value::Function(Box::new(f.clone())),
            (_, _) => unreachable!("Undefined identifier: {}", name),
        },
        CheckedAst::Assignment { target, expr, .. } => {
            let value = eval_expr(expr, env)?;
            eval_assignment(target, &value, env)?;
            value
        }
        CheckedAst::List { exprs, ty, .. } => Value::List(
            exprs
                .iter()
                .map(|e| eval_expr(e, env))
                .collect::<Result<Vec<Value>, _>>()?,
            ty.clone(),
        ),
        CheckedAst::Record { fields, ty, .. } => {
            let mut record = Vec::new();
            for (key, value) in fields {
                let value = eval_expr(value, env)?;
                record.push((key.clone(), value));
            }
            Value::Record(record, ty.clone())
        }
        CheckedAst::FieldAccess { expr, field, .. } => {
            let record = eval_expr(expr, env)?;
            match record {
                Value::Record(fields, _) => fields
                    .iter()
                    .find(|(key, _)| key == field)
                    .map(|(_, value)| value.clone())
                    .unwrap_or_else(|| {
                        unreachable!("This should have been checked by the type checker")
                    }),
                _ => {
                    unreachable!("This should have been checked by the type checker");
                }
            }
        }
        CheckedAst::FunctionDef {
            param,
            body,
            return_type,
            ..
        } => Value::Function(Box::new(Function::new_user(
            param.clone(),
            *body.clone(),
            env.deep_clone(),
            return_type.clone(),
        ))),
        CheckedAst::Block { exprs, .. } => {
            let mut result = Value::Unit;
            let mut scope = env.new_child(Str::Str("<block>"));
            for expr in exprs {
                result = eval_expr(expr, &mut scope)?;
            }
            result
        }
    };
    if !matches!(ast, CheckedAst::Literal { .. }) {
        log::trace!(
            "Eval: {} -> {}",
            ast.print_sexpr(),
            result.pretty_print_color()
        );
    }
    Ok(result)
}

fn eval_call(
    function: &CheckedAst,
    arg: &CheckedAst,
    env: &mut Environment,
    info: &LineInfo,
) -> InterpretResult {
    // First try to unwrap any native function call
    if let Some((native, args)) = unwrap_native_call(function, vec![arg], env) {
        // We only allow fully-applied native functions
        if args.len() != native.params.len() {
            log::error!(
                "Expected {} arguments, found {} when calling native function {}",
                native.params.len(),
                args.len(),
                native.name.clone()
            );
            unreachable!("This should have been checked by the type checker");
        }
        // Extract the handler from the native function,
        // so that the lifetime of the `native` ref is
        // dropped before we use `env` again to evaluate
        // the arguments.
        let handler = native.handler;
        let mut args = args
            .iter()
            .map(|arg| eval_expr(arg, env))
            .collect::<Result<Vec<Value>, _>>()?;
        // Invoke the native function
        return handler(&mut args, info);
    }

    // TODO: Implement support for function overloading (multiple variations)

    let function = eval_expr(function, env)?;
    let Value::Function(function) = function else {
        unreachable!("This should have been checked by the type checker");
    };

    // Evaluate the function body or invoke the native handler
    match function.borrow() {
        Function::User(UserFunction { body, param, .. }) => {
            let arg = eval_expr(arg, env)?;
            let mut closure = env.new_child(Str::Str("<closure>"));
            // Bind the argument to the parameter of the function variation
            closure.add_value(Str::String(param.name.clone()), arg.clone(), info)?;
            eval_expr(body, &mut closure)
        }
        Function::Native { .. } => {
            unreachable!("Native functions must not reach this!!!");
        }
    }
}

/// Unwrap a native function call
/// Returns a tuple of the native function and the arguments
/// If the expression is not a native function, return None
///
/// ## Memory
/// This function is recursive and will consume stack memory
/// proportional to the depth of the function call.
/// However it will *not* consume any heap memory due to the
/// use of references.
fn unwrap_native_call<'a, 'b>(
    expr: &'b CheckedAst,
    mut args: Vec<&'b CheckedAst>,
    env: &'a mut Environment,
) -> Option<(&'a NativeFunction, Vec<&'b CheckedAst>)> {
    match expr {
        CheckedAst::Identifier { name, .. } => match env.lookup_function(name) {
            Some(Function::Native(native)) => Some((native, args)),
            _ => None,
        },
        CheckedAst::FunctionCall { expr, arg, .. } => {
            // This argument will be applied before the other arguments
            args.insert(0, arg);
            // Recurse until we find a native function (if any)
            unwrap_native_call(expr, args, env)
        }
        _ => None,
    }
}

/// Assume `elems` are a non-empty vector
fn eval_tuple(exprs: &[CheckedAst], env: &mut Environment) -> InterpretResult {
    if exprs.is_empty() {
        return Ok(Value::Unit);
    }
    let (values, types): (Vec<Value>, Vec<Type>) = exprs
        .iter()
        .map(|e| {
            let value = eval_expr(e, env)?;
            let ty = value.get_type().clone();
            Ok((value, ty))
        })
        .collect::<Result<Vec<(Value, Type)>, _>>()?
        .into_iter()
        .unzip();

    Ok(Value::Tuple(values, Type::Tuple(types)))
}

/// Evaluate an assignment expression to a bind pattern
fn eval_assignment(
    pattern: &BindPattern,
    value: &Value,
    env: &mut Environment,
) -> Failable<RuntimeError> {
    match pattern {
        // BindPattern::Function { name, params, .. } => {
        //     let mut closure = env.new_child(Str::Str("<closure>"));
        //     closure.add_value(name.clone(), value.clone(), pattern.info())?;
        //     for param in params {
        //         if let BindPattern::Variable { name, .. } = param {
        //             closure.add_value(name.clone(), value.clone(), pattern.info())?;
        //         }
        //     }
        // }
        BindPattern::Variable { name, .. } => {
            env.add_value(Str::String(name.clone()), value.clone(), pattern.info())?
        }
        BindPattern::Tuple { elements, .. } => {
            let Value::Tuple(values, _) = value else {
                unreachable!("This should have been checked by the type checker");
            };
            for (pattern, value) in elements.iter().zip(values) {
                eval_assignment(pattern, value, env)?;
            }
        }
        BindPattern::Record { fields, .. } => {
            let Value::Record(values, _) = value else {
                unreachable!("This should have been checked by the type checker");
            };
            for (key, pattern) in fields {
                let Some((_, value)) = values.iter().find(|(k, _)| k == key) else {
                    unreachable!("This should have been checked by the type checker");
                };
                eval_assignment(pattern, value, env)?;
            }
        }
        BindPattern::List { elements, .. } => {
            let Value::List(values, _) = value else {
                unreachable!("This should have been checked by the type checker");
            };
            for (element, value) in elements.iter().zip(values) {
                eval_assignment(element, value, env)?;
            }
        }
        BindPattern::Wildcard => {}
        BindPattern::Literal { value: lit, .. } => {
            if value != lit {
                return Err(RuntimeError::new(
                    format!(
                        "Literal pattern match failed: expected {}, found {}",
                        lit.pretty_print(),
                        value.pretty_print()
                    ),
                    pattern.info().clone(),
                ));
            }
        }
        BindPattern::Rest { .. } => {
            // Handle rest pattern if needed
        }
        BindPattern::Function { .. } => {
            unreachable!("Function binding patterns are not supported in this context");
        }
    }
    Ok(())
}
