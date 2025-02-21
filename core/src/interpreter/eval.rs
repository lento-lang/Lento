use std::borrow::Borrow;

use crate::{
    interpreter::value::NativeFunction,
    type_checker::{
        checked_ast::{CheckedAst, CheckedModule},
        types::{GetType, Type},
    },
    util::str::Str,
};

use super::{
    env::Environment,
    error::{runtime_error, RuntimeError},
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
        CheckedAst::Call { function, arg, .. } => eval_call(function, arg, env)?,
        CheckedAst::Tuple(v, _, _) => eval_tuple(v, env)?,
        CheckedAst::Literal(l, _) => l.clone(),
        CheckedAst::Identifier(id, _, _) => match env.lookup_identifier(id) {
            (Some(_), Some(_)) => {
                return Err(runtime_error(format!("Ambiguous identifier '{}'", id)))
            }
            (Some(v), _) => v.clone(),
            (_, Some(f)) => Value::Function(Box::new(f.clone())),
            (None, None) => return Err(runtime_error(format!("Unknown identifier '{}'", id))),
        },
        CheckedAst::Assignment(lhs, rhs, _, _) => {
            let lhs = match *lhs.to_owned() {
                CheckedAst::Identifier(id, _, _) => id,
                _ => {
                    return Err(runtime_error(
                        "Assignment expects an identifier".to_string(),
                    ))
                }
            };
            let rhs = eval_ast(rhs, env)?;
            env.add_value(Str::String(lhs), rhs.clone())?;
            rhs
        }
        CheckedAst::List(elems, ty, _) => {
            let values = elems
                .iter()
                .map(|e| eval_ast(e, env))
                .collect::<Result<Vec<Value>, _>>()?;
            Value::List(values, ty.clone())
        }
        CheckedAst::Record(expr, ty, _) => {
            let mut record = Vec::new();
            for (key, value) in expr {
                let value = eval_ast(value, env)?;
                record.push((key.clone(), value));
            }
            Value::Record(record, ty.clone())
        }
        CheckedAst::Function(func, _) => Value::Function(Box::new(Function::new_user(
            func.param.clone(),
            func.body.clone(),
            env.deep_clone(),
            func.return_type.clone(),
        ))),
        CheckedAst::Block(exprs, _, _) => {
            let mut result = Value::Unit;
            let mut scope = env.new_child(Str::Str("<block>"));
            for expr in exprs {
                result = eval_ast(expr, &mut scope)?;
            }
            result
        }
    };
    if !matches!(ast, CheckedAst::Literal(_, _)) {
        log::trace!(
            "Eval: {} -> {}",
            ast.print_sexpr(),
            result.pretty_print_color()
        );
    }
    Ok(result)
}

fn eval_call(function: &CheckedAst, arg: &CheckedAst, env: &mut Environment) -> InterpretResult {
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
            CheckedAst::Identifier(id, _, _) => match env.lookup_function(id) {
                Some(Function::Native(native)) => Some((native, args)),
                _ => None,
            },
            CheckedAst::Call { function, arg, .. } => {
                // This argument will be applied before the other arguments
                args.insert(0, arg);
                // Recurse until we find a native function (if any)
                unwrap_native(function, args, env)
            }
            _ => None,
        }
    }

    // First try to unwrap any native function call
    if let Some((native, args)) = unwrap_native(function, vec![arg], env) {
        // We only allow fully-applied native functions
        if args.len() != native.params.len() {
            return Err(runtime_error(format!(
                "Expected {} arguments, found {} when calling native function '{}'",
                native.params.len(),
                args.len(),
                native.name
            )));
        }
        // Extract the handler from the native function,
        // so that the lifetime of the `native` ref is
        // dropped before we use `env` again to evaluate
        // the arguments.
        let handler = native.handler;
        let args = args
            .iter()
            .map(|arg| eval_ast(arg, env))
            .collect::<Result<Vec<Value>, _>>()?;
        // Invoke the native function
        return handler(args);
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
            closure.add_value(Str::String(param.name.clone()), arg.clone())?;
            eval_ast(body, &mut closure)
        }
        Function::Native { .. } => {
            unreachable!("Native functions must not reach this!!!");
        }
    }
}

/// Assume `elems` are a non-empty vector
fn eval_tuple(elems: &[CheckedAst], env: &mut Environment) -> InterpretResult {
    if elems.is_empty() {
        return Ok(Value::Unit);
    }
    let (values, types): (Vec<Value>, Vec<Type>) = elems
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
