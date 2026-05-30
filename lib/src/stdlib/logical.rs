use crate::{
    interpreter::{
        error::RuntimeError,
        value::{Function, Value},
    },
    type_checker::{checked_ast::CheckedParam, types::std_types},
    util::error::BaseErrorExt,
};

pub fn eq() -> Function {
    Function::new_native(
        "eq".into(),
        |values, info| {
            if values.len() != 2 {
                return Err(RuntimeError::new(
                    "eq() expects 2 arguments".to_string(),
                    info.clone(),
                ));
            }
            Ok(Value::Boolean(values[0] == values[1]))
        },
        vec![
            CheckedParam::from_str("lhs", std_types::ANY),
            CheckedParam::from_str("rhs", std_types::ANY),
        ],
        std_types::BOOL,
    )
}
