use crate::{
    interpreter::{
        number::{Number, SignedInteger},
        value::{Function, Value},
    },
    type_checker::{
        checked_ast::CheckedParam,
        types::{std_types, GetType},
    },
};

//--------------------------------------------------------------------------------------//
//                               Native Runtime Functions                               //
//--------------------------------------------------------------------------------------//

/// Print the given values to the console with a newline at the end.
pub fn print() -> Function {
    Function::new_native(
        "print".into(),
        |values| {
            for val in values {
                println!("{}", val.pretty_print());
            }
            Ok(Value::Unit)
        },
        vec![CheckedParam {
            name: "values".to_string(),
            ty: std_types::ANY,
        }],
        std_types::UNIT,
    )
}

/// Return the type of a value.
pub fn type_of() -> Function {
    Function::new_native(
        "typeof".into(),
        |values| {
            if values.len() != 1 {
                panic!("typeof() expects 1 argument");
            }
            Ok(Value::Type(values[0].get_type().clone()))
        },
        vec![CheckedParam {
            name: "value".to_string(),
            ty: std_types::ANY,
        }],
        std_types::TYPE,
    )
}

/// Exit the program with the given exit code.
pub fn exit() -> Function {
    Function::new_native(
        "exit".into(),
        |values| {
            if values.len() != 1 {
                panic!("exit() expects 1 argument");
            }
            match &values[0] {
                Value::Number(n) => {
                    if let Number::SignedInteger(SignedInteger::Int32(code)) = n {
                        std::process::exit(*code);
                    } else {
                        panic!(
                            "exit() expects 1 argument of type '{}', got '{}'",
                            std_types::INT32,
                            n.get_type()
                        );
                    }
                }
                _ => panic!(
                    "exit() expects 1 argument of type '{}', got '{}'",
                    std_types::INT32,
                    values[0].get_type()
                ),
            }
        },
        vec![CheckedParam {
            name: "code".to_string(),
            ty: std_types::INT32,
        }],
        std_types::UNIT,
    )
}
