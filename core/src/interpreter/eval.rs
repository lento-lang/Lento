use std::borrow::Borrow;

use crate::{
    interpreter::value::NativeFunction,
    lexer::token::LineInfo,
    type_checker::{
        checked_ast::{CheckedAst, CheckedModule},
        types::{GetType, Type},
    },
    util::str::Str,
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
pub fn eval_module(module: &CheckedModule, env: &mut Environment) -> InterpretResult {
    let mut result = Value::Unit;
    for expr in &module.expressions {
        result = eval_ast(expr, env)?;
    }
    Ok(result)
}

/// Interpret a type-checked AST node
pub fn eval_ast(ast: &CheckedAst, env: &mut Environment) -> InterpretResult {
    let result = match ast {
        CheckedAst::FunctionCall {
            expr: function,
            arg,
            ..
        } => eval_call(function, arg, env, ast.info())?,
        CheckedAst::Tuple { exprs, .. } => eval_tuple(exprs, env)?,
        CheckedAst::Literal { value, .. } => value.clone(),
        CheckedAst::Identifier { name, .. } => match env.lookup_identifier(name) {
            (Some(_), Some(_)) => {
                return Err(RuntimeError::new(
                    format!("Ambiguous identifier '{}'", name),
                    ast.info().clone(),
                ))
            }
            (Some(v), _) => v.clone(),
            (_, Some(f)) => Value::Function(Box::new(f.clone())),
            (None, None) => {
                return Err(RuntimeError::new(
                    format!("Unknown identifier '{}'", name),
                    ast.info().clone(),
                ))
            }
        },
        CheckedAst::Assignment { target, expr, .. } => {
            let info = target.info();
            let target = match *target.to_owned() {
                CheckedAst::Identifier { name, .. } => name,
                _ => {
                    return Err(RuntimeError::new(
                        "Assignment expects an identifier".to_string(),
                        ast.info().clone(),
                    ))
                }
            };
            let value = eval_ast(expr, env)?;
            env.add_value(Str::String(target), value.clone(), info)?;
            value
        }
        CheckedAst::List { exprs, ty, .. } => Value::List(
            exprs
                .iter()
                .map(|e| eval_ast(e, env))
                .collect::<Result<Vec<Value>, _>>()?,
            ty.clone(),
        ),
        CheckedAst::Record { fields, ty, .. } => {
            let mut record = Vec::new();
            for (key, value) in fields {
                let value = eval_ast(value, env)?;
                record.push((key.clone(), value));
            }
            Value::Record(record, ty.clone())
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
                result = eval_ast(expr, &mut scope)?;
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
    /// Unwrap a native function call
    /// Returns a tuple of the native function and the arguments
    /// If the expression is not a native function, return None
    ///
    /// ## Memory
    /// This function is recursive and will consume stack memory
    /// proportional to the depth of the function call.
    /// However it will *not* consume any heap memory due to the
    /// use of references.
    fn unwrap_native<'a, 'b>(
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
                unwrap_native(expr, args, env)
            }
            _ => None,
        }
    }

    // First try to unwrap any native function call
    if let Some((native, args)) = unwrap_native(function, vec![arg], env) {
        // We only allow fully-applied native functions
        if args.len() != native.params.len() {
            return Err(RuntimeError::new(
                format!(
                    "Expected {} arguments, found {} when calling native function '{}'",
                    native.params.len(),
                    args.len(),
                    native.name
                ),
                info.clone(),
            ));
        }
        // Extract the handler from the native function,
        // so that the lifetime of the `native` ref is
        // dropped before we use `env` again to evaluate
        // the arguments.
        let handler = native.handler;
        let mut args = args
            .iter()
            .map(|arg| eval_ast(arg, env))
            .collect::<Result<Vec<Value>, _>>()?;
        // Invoke the native function
        return handler(&mut args, info);
    }

    // TODO: Implement support for function overloading (multiple variations)

    let function = eval_ast(function, env)?;
    let Value::Function(function) = function else {
        unreachable!("This should have been checked by the type checker");
    };

    // Evaluate the function body or invoke the native handler
    match function.borrow() {
        Function::User(UserFunction { body, param, .. }) => {
            let arg = eval_ast(arg, env)?;
            let mut closure = env.new_child(Str::Str("<closure>"));
            // Bind the argument to the parameter of the function variation
            closure.add_value(Str::String(param.name.clone()), arg.clone(), info)?;
            eval_ast(body, &mut closure)
        }
        Function::Native { .. } => {
            unreachable!("Native functions must not reach this!!!");
        }
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
            let value = eval_ast(e, env)?;
            let ty = value.get_type().clone();
            Ok((value, ty))
        })
        .collect::<Result<Vec<(Value, Type)>, _>>()?
        .into_iter()
        .unzip();

    Ok(Value::Tuple(values, Type::Tuple(types)))
}
