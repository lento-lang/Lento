use crate::{
    interpreter::{
        error::runtime_error,
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
        |values| {
            let ty_num = std_types::NUM();
            if values.len() != 2 {
                return Err(runtime_error("add() expects 2 arguments".to_string()));
            }
            let lhs = values[0].clone();
            let rhs = values[1].clone();
            if lhs.get_type().subtype(&ty_num).success && rhs.get_type().subtype(&ty_num).success {
                match (lhs, rhs) {
                    (Value::Number(l), Value::Number(r)) => Ok(Value::Number(Number::add(&l, &r))),
                    _ => panic!("add expects 2 arguments of type '{}'", ty_num),
                }
            } else {
                Err(runtime_error(format!(
                    "add expects 2 arguments of type '{}', got '{}' and '{}'",
                    ty_num,
                    lhs.get_type(),
                    rhs.get_type()
                )))
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
        |values| {
            let ty_num = std_types::NUM();
            if values.len() != 2 {
                return Err(runtime_error("sub() expects 2 arguments".to_string()));
            }
            let lhs = values[0].clone();
            let rhs = values[1].clone();
            if lhs.get_type().subtype(&ty_num).success && rhs.get_type().subtype(&ty_num).success {
                match (lhs, rhs) {
                    (Value::Number(l), Value::Number(r)) => Ok(Value::Number(Number::sub(&l, &r))),
                    _ => panic!("sub expects 2 arguments of type '{}'", ty_num),
                }
            } else {
                Err(runtime_error(format!(
                    "sub expects 2 arguments of type '{}', got '{}' and '{}'",
                    ty_num,
                    lhs.get_type(),
                    rhs.get_type()
                )))
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
        |values| {
            let ty_num = std_types::NUM();
            if values.len() != 2 {
                return Err(runtime_error("mul() expects 2 arguments".to_string()));
            }
            let lhs = values[0].clone();
            let rhs = values[1].clone();
            if lhs.get_type().subtype(&ty_num).success && rhs.get_type().subtype(&ty_num).success {
                match (lhs, rhs) {
                    (Value::Number(l), Value::Number(r)) => Ok(Value::Number(Number::mul(&l, &r))),
                    _ => panic!("mul expects 2 arguments of type '{}'", ty_num),
                }
            } else {
                Err(runtime_error(format!(
                    "mul expects 2 arguments of type '{}', got '{}' and '{}'",
                    ty_num,
                    lhs.get_type(),
                    rhs.get_type()
                )))
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
        |values| {
            let ty_num = std_types::NUM();
            if values.len() != 2 {
                return Err(runtime_error("div() expects 2 arguments".to_string()));
            }
            let lhs = values[0].clone();
            let rhs = values[1].clone();
            if lhs.get_type().subtype(&ty_num).success && rhs.get_type().subtype(&ty_num).success {
                match (lhs, rhs) {
                    (Value::Number(l), Value::Number(r)) => Ok(Value::Number(Number::div(&l, &r)?)),
                    _ => panic!("div expects 2 arguments of type '{}'", ty_num),
                }
            } else {
                Err(runtime_error(format!(
                    "div expects 2 arguments of type '{}', got '{}' and '{}'",
                    ty_num,
                    lhs.get_type(),
                    rhs.get_type()
                )))
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
