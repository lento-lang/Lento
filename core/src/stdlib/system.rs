use crate::{
    interpreter::{
        number::{Number, SignedInteger},
        value::{Function, Value},
    },
    type_checker::{
        checked_ast::CheckedParam,
        types::{std_types, GetType, Type},
    },
};

//--------------------------------------------------------------------------------------//
//                               Native Runtime Functions                               //
//--------------------------------------------------------------------------------------//

/// Print the given values to the console with a newline at the end.
pub fn print() -> Function {
    Function::new_native(
        "print".into(),
        |values, _| {
            for val in values {
                println!("{}", val.pretty_print());
            }
            Ok(Value::Unit)
        },
        vec![CheckedParam::from_str("values", Type::Variable("T".into()))],
        std_types::UNIT,
    )
}

pub fn dbg() -> Function {
    Function::new_native(
        "dbg".into(),
        |values, _| {
            if values.len() != 1 {
                panic!("dbg() expects 1 argument");
            }
            let value = values.pop().unwrap();
            println!(
                "<{}:{}>",
                value.get_type().pretty_print_color(),
                value.pretty_print_color()
            );
            Ok(value)
        },
        vec![CheckedParam::from_str("values", Type::Variable("T".into()))],
        Type::Variable("T".into()),
    )
}

/// Return the type of a value.
pub fn type_of() -> Function {
    Function::new_native(
        "typeof".into(),
        |values, _| {
            if values.len() != 1 {
                panic!("typeof() expects 1 argument");
            }
            Ok(Value::Type(values[0].get_type().clone()))
        },
        vec![CheckedParam::from_str("value", std_types::ANY)],
        std_types::TYPE,
    )
}

/// Exit the program with the given exit code.
pub fn exit() -> Function {
    Function::new_native(
        "exit".into(),
        |values, _| {
            if values.len() != 1 {
                panic!("exit() expects 1 argument");
            }
            match &values[0] {
                Value::Number(n) => {
                    if let Number::SignedInteger(SignedInteger::Int32(code)) = n {
                        std::process::exit(*code);
                    } else {
                        panic!(
                            "exit() expects 1 argument of type {}, but got {}",
                            std_types::INT32.pretty_print_color(),
                            n.get_type().pretty_print_color()
                        );
                    }
                }
                _ => panic!(
                    "exit() expects 1 argument of type {}, got {}",
                    std_types::INT32.pretty_print_color(),
                    values[0].get_type().pretty_print_color()
                ),
            }
        },
        vec![CheckedParam::from_str("code", std_types::INT32)],
        std_types::UNIT,
    )
}
