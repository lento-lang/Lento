use crate::{
    parser::{
        ast::Ast,
        error::ParseError,
        op::OpInfo,
        parser::{ParseResult, ASSIGNMENT_SYM, COMMA_SYM, FN_ARROW_SYM, SUM_TYPE_SYM},
        pattern::BindPattern,
    },
    type_checker::checked_ast::{ParamAst, TypeAst},
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
pub fn top(expr: Ast, types: &HashSet<String>, variables: Option<&HashSet<String>>) -> ParseResult {
    match expr {
        // Specialize type constructors to literal types
        Ast::FunctionCall { expr, arg, info } => call(*expr, *arg, info, types, variables),
        // Specialize assignments to binding patterns with optional type annotations
        Ast::Binary {
            lhs,
            op: op_info,
            rhs,
            info,
        } if op_info.symbol == ASSIGNMENT_SYM => assignment(*lhs, *rhs, info, types, variables),
        Ast::Binary {
            lhs,
            op: op_info,
            rhs,
            info,
            ..
        } if op_info.symbol == COMMA_SYM => {
            comma_sequence(*lhs, op_info, *rhs, info, types, variables)
        }
        // No specialization available
        _ => Ok(expr),
    }
}

/// Specialize a comma-separated sequence of expressions into a more specific expression.
/// E.g:
/// - `f x, y, z {...}` becomes `f x, y, z = {...}` if `f` is a function definition.
pub fn comma_sequence(
    expr: Ast,
    op_info: OpInfo,
    rhs: Ast,
    info: LineInfo,
    types: &HashSet<String>,
    variables: Option<&HashSet<String>>,
) -> ParseResult {
    log::trace!("Specializing comma sequence: {:?}, {:?}", expr, rhs);
    if let Some(res) = block_def_comma_sequence(&expr, op_info, &rhs, &info, types, variables) {
        return res;
    }
    let mut exprs = flatten_sequence(expr);
    exprs.push(rhs);
    Ok(Ast::Tuple {
        info: info.join(exprs.last().unwrap().info()),
        exprs,
    })
}

// Specialize function definitions with blocks
/// This is used to handle function definitions like:
/// ```ignore
/// f x, y, z {...}
/// List int f str x, int y {...}
/// ```
/// Complex types can also be used here as seen above.
pub fn block_def_comma_sequence(
    expr: &Ast,
    op_info: OpInfo,
    rhs: &Ast,
    info: &LineInfo,
    types: &HashSet<String>,
    variables: Option<&HashSet<String>>,
) -> Option<ParseResult> {
    if variables.is_some() && matches!(rhs, Ast::FunctionCall { .. }) {
        let Ast::FunctionCall {
            expr: last_expr,
            arg: last_arg,
            ..
        } = rhs
        else {
            unreachable!();
        };
        if matches!(**last_arg, Ast::Block { .. }) {
            // If the outer arg is a block, we can potentially specialize it as a function definition with a block.
            log::trace!("Trying to specialize potential function definition with block");
            let res = assignment(
                Ast::Binary {
                    lhs: Box::new(expr.clone()),
                    op: op_info,
                    rhs: last_expr.clone(),
                    info: info.clone(),
                },
                *last_arg.clone(),
                info.clone(),
                types,
                variables,
            );
            if res.is_ok() {
                log::trace!("Specialized function definition successfully!");
                return Some(res);
            } else {
                log::trace!("Failed to specialize function definition: {:?}", res);
            }
        }
    }
    None
}

// Specialize type constructors to literal types
pub fn call(
    expr: Ast,
    arg: Ast,
    info: LineInfo,
    types: &HashSet<String>,
    variables: Option<&HashSet<String>>,
) -> ParseResult {
    if let Some(res) = block_def_call(&expr, &arg, &info, types, variables) {
        return res;
    }
    Ok(Ast::FunctionCall {
        expr: Box::new(expr),
        arg: Box::new(arg),
        info,
    })
}

// Specialize function definitions with blocks
/// This is used to handle function definitions like:
/// ```ignore
/// f() {...}
/// f(x) {...}
/// bool f() {...}
/// int f str x {...}
/// Map int str f int x {...}
/// ```
/// Complex types can also be used here as seen above.
pub fn block_def_call(
    expr: &Ast,
    arg: &Ast,
    info: &LineInfo,
    types: &HashSet<String>,
    variables: Option<&HashSet<String>>,
) -> Option<ParseResult> {
    if variables.is_some() && matches!(arg, Ast::Block { .. }) {
        // Check if function definition with block:
        // `f() {...}`, `bool f() {...}`, `f(x) {...}` or `int f str x {...}` etc.
        log::trace!("Trying to specializing potential function definition with block");
        let res = assignment(expr.clone(), arg.clone(), info.clone(), types, variables);
        if res.is_ok() {
            log::trace!("Specialized function definition successfully!");
            return Some(res);
        } else {
            log::trace!("Failed to specialize function definition: {:?}", res);
        }
    }
    None
}

/// Convert a loose AST expression into a binding pattern.
pub fn assignment(
    target: Ast,
    body: Ast,
    assignment_info: LineInfo,
    types: &HashSet<String>,
    variables: Option<&HashSet<String>>,
) -> ParseResult {
    log::debug!(
        "Specializing assignment: {} = {}",
        target.print_expr(),
        body.print_expr()
    );
    log::trace!("Specializing assignment: {:?} = {:?}", target, body);
    match &target {
        // `x, y = ...`, `f x, y = ...`, `f int x, bool y = ...` or `int f int x, bool y = ...` etc.
        // Potentially a function definition.
        Ast::Binary { op: op_info, .. } if op_info.symbol == COMMA_SYM => definition(
            flatten_sequence(target)
                .into_iter()
                .flat_map(flatten_calls)
                .collect::<Vec<Ast>>(),
            body,
            types,
            variables,
        ),
        // `int x = ...`, `f x = ...`, `f int x = ...`, `int f x = ...` or `int f int x = ...`.
        // Complex types can also be used here, like `List int list1 = ...` or `Map int str map1 = ...`.
        // Potentially a function definition.
        Ast::FunctionCall { .. } => definition(flatten_calls(target), body, types, variables),
        // Try parse other generic binding patterns (non-typed) for assignments like:
        // `_ = ...`, `x = ...`, `[x, y] = ...`, `{ a: x, b: y } = ...`, etc.
        _ => Ok(Ast::Let {
            target: BindPattern::from_expr(target)?,
            expr: Box::new(body),
            annotation: None,
            info: assignment_info,
        }),
    }
}

/// Parse a variable or function definition unless the variable is already defined (known per scope).
/// This is used to handle function definitions like:
/// ```ignore
/// x ...
/// int x ...
/// f x ...
/// f int x ...
/// int f x ...
/// int f int x ...
/// List int f int x ...
/// Map int str f int x ...
/// f x, y ...
/// f x, y, z ...
/// int f x, y ...
/// int f int x, y ...
/// Map int str f int x, List int y ...
/// ```
/// Complex types can also be used here as seen above.
pub fn definition(
    mut exprs: Vec<Ast>,
    body: Ast,
    types: &HashSet<String>,
    variables: Option<&HashSet<String>>,
) -> ParseResult {
    log::trace!(
        "Parsing function definition start from expressions: {:?}",
        exprs
            .iter()
            .map(|e| e.print_expr())
            .collect::<Vec<String>>()
    );
    let func = next_typed_binding_pattern(&mut exprs, types)?;
    let BindPattern::Variable {
        name,
        info: name_info,
    } = func.pattern
    else {
        return Err(ParseError::new(
            format!(
                "Invalid function binding pattern: {}",
                func.pattern.print_expr()
            ),
            func.pattern.info().clone(),
        )
        .with_label(
            format!(
                "Expected a function name, found: {}",
                func.pattern.print_expr()
            ),
            func.pattern.info().clone(),
        ));
    };
    log::trace!("Found variable or function name: {}", name.clone().yellow());
    if let Some(variables) = variables {
        // Check if the variable is already defined in the current scope
        // Otherwise create a new variable binding
        log::trace!(
            "Checking if variable `{}` is already defined in the current scope",
            name
        );
        if variables.contains(&name) {
            return Err(ParseError::new(
                format!("Variable `{}` is already defined", name),
                name_info.clone(),
            )
            .with_label("This already exist".to_string(), name_info.clone()));
        }
    }
    if exprs.is_empty() {
        // `int x = ...`
        // If no parameters are found, it's a typed variable binding
        log::trace!("Found a typed variable binding: {} = ...", name);
        Ok(Ast::Let {
            info: name_info.clone(),
            annotation: func.ty,
            target: BindPattern::Variable {
                name,
                info: name_info,
            },
            expr: Box::new(body),
        })
    } else {
        // `f x = ...`, `int f x, y = ...`, or `int f int x, bool y, str z = ...`, etc.
        let mut params = Vec::new();
        while !exprs.is_empty() {
            // Parse the next (possibly typed) parameter from the remaining expressions
            let param = next_typed_binding_pattern(&mut exprs, types)?;
            log::trace!(
                "Found function parameter: {}{}",
                if let Some(annotation) = &param.ty {
                    format!("{} ", annotation.pretty_print().light_blue())
                } else {
                    "".to_string()
                },
                param.pattern.pretty_print()
            );
            params.push(param);
        }
        // Create a function definition via nested lambda expressions assigned to a variable
        log::trace!(
            "Creating function definition for {} with parameters: {:?}",
            name,
            params
                .iter()
                .map(|p| p.pattern.pretty_print())
                .collect::<Vec<String>>()
        );
        let info = name_info.join(body.info());
        Ok(create_function_assignment(
            func.ty, name, name_info, params, body, info,
        ))
    }
}

/// Create a function definition via nested lambda expressions assigned to a variable.
pub fn create_function_assignment(
    annotation: Option<TypeAst>,
    name: String,
    name_info: LineInfo,
    params: Vec<ParamAst>,
    body: Ast,
    info: LineInfo,
) -> Ast {
    log::trace!(
        "Creating function {}{}({}) = {}",
        if let Some(annotation) = &annotation {
            format!("{} ", annotation.pretty_print().light_blue())
        } else {
            "".to_string()
        },
        name,
        params
            .iter()
            .map(|p| format!(
                "{}{}",
                if let Some(annotation) = &p.ty {
                    format!("{} ", annotation.pretty_print().light_blue())
                } else {
                    "".to_string()
                },
                p.pattern.pretty_print()
            ))
            .collect::<Vec<String>>()
            .join(", "),
        body.print_expr()
    );
    let mut params = params.into_iter().rev();
    let first = params.next().unwrap();
    let mut lambda = Ast::Lambda {
        info: first.pattern.info().join(body.info()),
        param: Box::new(Ast::Identifier {
            name: first.pattern.print_expr(),
            info: first.pattern.info().clone(),
        }),
        body: Box::new(body),
        return_type: None,
    };
    for param in params {
        lambda = Ast::Lambda {
            info: lambda.info().join(param.pattern.info()),
            param: Box::new(Ast::Identifier {
                name: param.pattern.print_expr(),
                info: param.pattern.info().clone(),
            }),
            body: Box::new(lambda),
            return_type: None,
        };
    }
    Ast::Let {
        info,
        annotation,
        target: BindPattern::Variable {
            name,
            info: name_info,
        },
        expr: Box::new(lambda),
    }
}

//--------------------------------------------------------------------------------------//
//                                     Utilities                                        //
//--------------------------------------------------------------------------------------//

pub fn flatten_calls(expr: Ast) -> Vec<Ast> {
    let mut exprs = Vec::new();
    let mut queue = vec![expr];
    while let Some(current) = queue.pop() {
        match current {
            // Flatten function calls into a list of expressions
            Ast::FunctionCall { expr, arg, .. } => {
                queue.push(*arg);
                queue.push(*expr);
            }
            _ => exprs.push(current),
        }
    }
    exprs
}

pub fn flatten_sequence(expr: Ast) -> Vec<Ast> {
    let mut exprs = Vec::new();
    let mut queue = vec![expr];
    while let Some(current) = queue.pop() {
        match current {
            // Flatten sequences of expressions
            Ast::Binary {
                lhs,
                op: op_info,
                rhs,
                ..
            } if op_info.symbol == COMMA_SYM => {
                queue.push(*rhs);
                queue.push(*lhs);
            }
            _ => exprs.push(current),
        }
    }
    exprs
}

/// Parse a typed binding pattern from a list of expressions, like:
/// ```ignore
/// int f str x, bool y
/// ```
/// Yields the first parameter with its type annotation, like:
/// ```ignore
/// ParamAst {
///    ty: Some(TypeAst::Identifier { name: "int".to_string(), info: ... }),
///    pattern: BindPattern::Variable {
///    name: "f".to_string(),
///        info: ...,
///   }
/// }
/// ```
/// And modifies the `exprs` vector to remove the first parameter and its type annotation.
/// So the remaining expressions will be the rest of the parameters, like:
/// ```ignore
/// str x, bool y
/// ```
pub fn next_typed_binding_pattern(
    exprs: &mut Vec<Ast>,
    types: &HashSet<String>,
) -> Result<ParamAst, ParseError> {
    if exprs.is_empty() {
        unreachable!("Expected at least one expression for a binding pattern");
    }
    let annotation = match try_type_annotation(exprs, types) {
        Some(Ok(annotation)) => Some(annotation),
        Some(Err(err)) => return Err(err),
        None => None,
    };
    if exprs.is_empty() {
        return Err(ParseError::new(
            "Expected a binding pattern".to_string(),
            annotation.unwrap().info().clone(),
        ));
    }
    Ok(ParamAst {
        ty: annotation,
        pattern: BindPattern::from_expr(exprs.remove(0))?,
    })
}

/// Try to parse a type annotation from a list of expressions given from a function definition assignment.
///
/// ## Example
/// ```ignore
/// int f str x, bool y
/// ```
/// Becomes: (`int`, `f(str(x)), bool(y)`)
///
/// ## Example
/// ```ignore
/// List int f int x
/// ```
/// Becomes: (`List(int)`, `f(int(x))`)
pub fn try_type_annotation(
    exprs: &mut Vec<Ast>,
    types: &HashSet<String>,
) -> Option<Result<TypeAst, ParseError>> {
    let mut annotations: Vec<&Ast> = Vec::new();
    let mut is_type_constructor = false;
    for expr in exprs.iter() {
        // Check if the expression is a type expression
        if !is_type_expr(expr, types) {
            break;
        }
        // If the expression is a type expression, we add it to the annotations
        annotations.push(expr);
        if annotations.is_empty() {
            // `(((HashMap int) str) f) int x`
            if let Ast::Identifier { name, .. } = expr {
                if types.contains(name) {
                    is_type_constructor = true;
                }
            }
        }
    }
    // Make immutable for the rest of the function
    let annotations = annotations;
    is_type_constructor = is_type_constructor && annotations.len() > 1; // A constructor must have at least one type argument

    // Create a type from the annotation expressions
    if annotations.is_empty() {
        return None;
    }
    // We have type annotation expressions! 🎉
    if is_type_constructor {
        // We have a type constructor, like `List int` or `Map str int`
        // The first expression is the type constructor name, the rest are type arguments
        let Ast::Identifier {
            name: constructor_name,
            info: constructor_info,
        } = annotations.first().unwrap()
        else {
            unreachable!();
        };
        let mut params = Vec::new();
        for param in annotations.iter().skip(1) {
            match into_type_ast((*param).clone()) {
                Ok(type_ast) => {
                    params.push(type_ast);
                }
                Err(err) => {
                    return Some(Err(err.with_label(
                        format!("Invalid type argument: {}", param.print_expr()),
                        param.info().clone(),
                    )));
                }
            }
        }
        let last_info = params
            .last()
            .map_or(constructor_info.clone(), |p| p.info().clone());
        let type_expr = TypeAst::Constructor {
            expr: Box::new(TypeAst::Identifier {
                name: constructor_name.clone(),
                info: constructor_info.clone(),
            }),
            params,
            info: constructor_info.join(&last_info),
        };
        // Remove the type annotation expressions from the list
        exprs.drain(0..annotations.len());
        Some(Ok(type_expr))
    } else {
        // No type constructor found, make sure there is only ONE type annotation expression
        if annotations.len() != 1 {
            let mut err = ParseError::new(
                "Expected a single type annotation expression".to_string(),
                annotations[0].info().clone(),
            );
            if let Some(invalid) = annotations.get(1..) {
                let first_info = annotations.first().unwrap().info();
                let last_info = invalid.last().unwrap().info();
                err = err.with_label("These are invalid".to_string(), first_info.join(last_info));
            }
            err = err.with_hint("Did you mean to define a variable or function?".to_string());

            return Some(Err(err));
        }
        match into_type_ast(annotations[0].clone()) {
            Ok(type_ast) => {
                // Remove the type annotation expressions from the list
                exprs.drain(0..annotations.len());
                Some(Ok(type_ast))
            }
            Err(err) => Some(Err(err.with_label(
                "Invalid type annotation".to_string(),
                annotations[0].info().clone(),
            ))),
        }
    }
}

/// Quick check if a single expression can be converted into a `TypeAst`.
pub fn is_type_expr(expr: &Ast, types: &HashSet<String>) -> bool {
    match expr {
        Ast::Identifier { name, .. } => types.contains(name),
        // Sum types like `int | str`
        Ast::Binary { lhs, op, rhs, .. } => {
            (op.symbol == SUM_TYPE_SYM || op.symbol == FN_ARROW_SYM)
                && is_type_expr(lhs, types)
                && is_type_expr(rhs, types)
        }
        Ast::List { exprs, .. } if exprs.len() == 1 => is_type_expr(&exprs[0], types),
        Ast::Literal { .. } => false,
        Ast::FunctionCall { expr, arg, .. } => {
            is_type_expr(expr, types) && is_type_expr(arg, types)
        }
        // Product type like `(int, str)`
        Ast::Tuple { exprs, .. } => exprs.iter().all(|e| is_type_expr(e, types)),
        // Product type like `{ a: int, b: str }`
        Ast::Record { fields, .. } => fields.iter().all(|(_, v)| is_type_expr(v, types)),
        _ => false,
    }
}

pub fn into_type_ast(expr: Ast) -> Result<TypeAst, ParseError> {
    log::trace!(
        "Converting expression into TypeAst: {}",
        expr.print_expr().light_blue()
    );
    match expr {
        Ast::Identifier { name, info } => Ok(TypeAst::Identifier {
            name: name.clone(),
            info: info.clone(),
        }),
        Ast::List { mut exprs, info } if exprs.len() == 1 => {
            let inner = into_type_ast(exprs.remove(0))?;
            Ok(TypeAst::Constructor {
                expr: Box::new(TypeAst::Identifier {
                    name: "List".to_string(),
                    info: info.clone(),
                }),
                params: vec![inner],
                info: info.clone(),
            })
        }
        Ast::Record { fields, info } => {
            let fields = fields
                .into_iter()
                .map(|(key, value)| Ok((key, into_type_ast(value)?)))
                .collect::<Result<Vec<_>, ParseError>>()?;
            Ok(TypeAst::Record { fields, info })
        }
        Ast::FunctionCall { expr, arg, info } => {
            let mut params = vec![into_type_ast(*arg)?];
            let mut head = *expr;
            while let Ast::FunctionCall { expr, arg, .. } = head {
                params.push(into_type_ast(*arg)?);
                head = *expr;
            }
            params.reverse();
            let head = into_type_ast(head)?;
            Ok(TypeAst::Constructor {
                expr: Box::new(head),
                params,
                info,
            })
        }
        Ast::Binary { lhs, op, rhs, info }
            if op.symbol == SUM_TYPE_SYM || op.symbol == FN_ARROW_SYM =>
        {
            Ok(TypeAst::Binary {
                lhs: Box::new(into_type_ast(*lhs)?),
                op,
                rhs: Box::new(into_type_ast(*rhs)?),
                info,
            })
        }
        _ => Err(ParseError::new(
            format!("Expected a type expression, found: {}", expr.print_expr()),
            expr.info().clone(),
        )),
    }
}

/// Takes a function name, a list of parameters and a body and rolls them into a single assignment expression.
/// Parameters are rolled into a nested function definition.
/// All parameters are sorted like:
/// ```lento
/// func(a, b, c) = expr
/// ```
/// becomes:
/// ```lento
/// func = a -> b -> c -> expr
/// ```
///
/// # Arguments
/// - `func_name` The name of the function
/// - `params` A list of parameters in left-to-right order: `a, b, c`
/// - `body` The body of the function
pub fn _roll_function_definition(params: Vec<ParamAst>, body: Ast) -> Ast {
    assert!(!params.is_empty(), "Expected at least one parameter");
    let body_info = body.info().clone();
    let mut params_iter = params.into_iter().rev();
    let first = params_iter.next().unwrap();
    let mut function = Ast::Lambda {
        param: Box::new(Ast::Identifier {
            name: first.pattern.print_expr(),
            info: first.pattern.info().clone(),
        }),
        body: Box::new(body),
        return_type: None,
        info: first.pattern.info().join(&body_info),
    };
    for param in params_iter {
        function = Ast::Lambda {
            info: function.info().join(param.pattern.info()),
            param: Box::new(Ast::Identifier {
                name: param.pattern.print_expr(),
                info: param.pattern.info().clone(),
            }),
            body: Box::new(function),
            return_type: None,
        };
    }
    function
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
pub fn roll_function_call(expr: Ast, args: Vec<Ast>, types: &HashSet<String>) -> Ast {
    let last_info = args
        .last()
        .map(|a| a.info().clone())
        .unwrap_or(expr.info().clone());
    let call_info = expr.info().join(&last_info);
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
