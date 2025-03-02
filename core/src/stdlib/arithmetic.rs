use crate::{
    interpreter::{
        error::RuntimeError,
        number::{ArithmeticOperations, Number},
        value::{Function, Value},
    },
    type_checker::{
        checked_ast::CheckedParam,
        types::{std_types, GetType, TypeTrait},
    },
};

//--------------------------------------------------------------------------------------//
//                               Native Runtime Functions                               //
//--------------------------------------------------------------------------------------//

pub fn add() -> Function {
    Function::new_native(
        "add".into(),
        |values, info| {
            let ty_num = std_types::NUM();
            if values.len() != 2 {
                return Err(RuntimeError::new(
                    "add() expects 2 arguments".to_string(),
                    info.clone(),
                ));
            }
            let lhs = values[0].clone();
            let rhs = values[1].clone();
            if lhs.get_type().subtype(&ty_num).success && rhs.get_type().subtype(&ty_num).success {
                match (lhs, rhs) {
                    (Value::Number(l), Value::Number(r)) => Ok(Value::Number(Number::add(&l, &r))),
                    _ => panic!(
                        "add expects 2 arguments of type {}",
                        ty_num.pretty_print_color()
                    ),
                }
            } else {
                Err(RuntimeError::new(
                    format!(
                        "add expects 2 arguments of type {}, but got {} and {}",
                        ty_num.pretty_print_color(),
                        lhs.get_type().pretty_print_color(),
                        rhs.get_type().pretty_print_color()
                    ),
                    info.clone(),
                ))
            }
        },
        vec![
            CheckedParam {
                name: "lhs".to_string(),
                ty: std_types::NUM(),
            },
            CheckedParam {
                name: "rhs".to_string(),
                ty: std_types::NUM(),
            },
        ],
        std_types::NUM(),
    )
}

pub fn sub() -> Function {
    Function::new_native(
        "sub".into(),
        |values, info| {
            let ty_num = std_types::NUM();
            if values.len() != 2 {
                return Err(RuntimeError::new(
                    "sub() expects 2 arguments".to_string(),
                    info.clone(),
                ));
            }
            let lhs = values[0].clone();
            let rhs = values[1].clone();
            if lhs.get_type().subtype(&ty_num).success && rhs.get_type().subtype(&ty_num).success {
                match (lhs, rhs) {
                    (Value::Number(l), Value::Number(r)) => Ok(Value::Number(Number::sub(&l, &r))),
                    _ => panic!(
                        "sub expects 2 arguments of type {}",
                        ty_num.pretty_print_color()
                    ),
                }
            } else {
                Err(RuntimeError::new(
                    format!(
                        "sub expects 2 arguments of type {}, got {} and {}",
                        ty_num.pretty_print_color(),
                        lhs.get_type().pretty_print_color(),
                        rhs.get_type().pretty_print_color()
                    ),
                    info.clone(),
                ))
            }
        },
        vec![
            CheckedParam {
                name: "lhs".to_string(),
                ty: std_types::NUM(),
            },
            CheckedParam {
                name: "rhs".to_string(),
                ty: std_types::NUM(),
            },
        ],
        std_types::NUM(),
    )
}

pub fn mul() -> Function {
    Function::new_native(
        "mul".into(),
        |values, info| {
            let ty_num = std_types::NUM();
            if values.len() != 2 {
                return Err(RuntimeError::new(
                    "mul() expects 2 arguments".to_string(),
                    info.clone(),
                ));
            }
            let lhs = values[0].clone();
            let rhs = values[1].clone();
            if lhs.get_type().subtype(&ty_num).success && rhs.get_type().subtype(&ty_num).success {
                match (lhs, rhs) {
                    (Value::Number(l), Value::Number(r)) => Ok(Value::Number(Number::mul(&l, &r))),
                    _ => panic!(
                        "mul expects 2 arguments of type {}",
                        ty_num.pretty_print_color()
                    ),
                }
            } else {
                Err(RuntimeError::new(
                    format!(
                        "mul expects 2 arguments of type {}, got {} and {}",
                        ty_num.pretty_print_color(),
                        lhs.get_type().pretty_print_color(),
                        rhs.get_type().pretty_print_color()
                    ),
                    info.clone(),
                ))
            }
        },
        vec![
            CheckedParam {
                name: "lhs".to_string(),
                ty: std_types::NUM(),
            },
            CheckedParam {
                name: "rhs".to_string(),
                ty: std_types::NUM(),
            },
        ],
        std_types::NUM(),
    )
}

pub fn div() -> Function {
    Function::new_native(
        "div".into(),
        |values, info| {
            let ty_num = std_types::NUM();
            if values.len() != 2 {
                return Err(RuntimeError::new(
                    "div() expects 2 arguments".to_string(),
                    info.clone(),
                ));
            }
            let lhs = values[0].clone();
            let rhs = values[1].clone();
            if lhs.get_type().subtype(&ty_num).success && rhs.get_type().subtype(&ty_num).success {
                match (lhs, rhs) {
                    (Value::Number(l), Value::Number(r)) => {
                        Ok(Value::Number(Number::div(&l, &r, info)?))
                    }
                    _ => panic!(
                        "div expects 2 arguments of type {}",
                        ty_num.pretty_print_color()
                    ),
                }
            } else {
                Err(RuntimeError::new(
                    format!(
                        "div expects 2 arguments of type {}, got {} and {}",
                        ty_num.pretty_print_color(),
                        lhs.get_type().pretty_print_color(),
                        rhs.get_type().pretty_print_color()
                    ),
                    info.clone(),
                ))
            }
        },
        vec![
            CheckedParam {
                name: "lhs".to_string(),
                ty: std_types::NUM(),
            },
            CheckedParam {
                name: "rhs".to_string(),
                ty: std_types::NUM(),
            },
        ],
        std_types::NUM(),
    )
}
