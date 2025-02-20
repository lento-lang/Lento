use crate::{
    interpreter::{
        error::runtime_error,
        value::{Function, Value},
    },
    type_checker::{checked_ast::CheckedParam, types::std_types},
};

pub fn eq() -> Function {
    Function::new_native(
        "eq".into(),
        |values| {
            if values.len() != 2 {
                return Err(runtime_error("eq() expects 2 arguments".to_string()));
            }
            Ok(Value::Boolean(values[0] == values[1]))
        },
        vec![
            CheckedParam {
                name: "lhs".to_string(),
                ty: std_types::ANY,
            },
            CheckedParam {
                name: "rhs".to_string(),
                ty: std_types::ANY,
            },
        ],
        std_types::BOOL,
    )
}
