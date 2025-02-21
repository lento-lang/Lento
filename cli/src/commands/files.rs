use std::{path::Path, process::exit};

use colorful::Colorful;

use lento_core::{
    interpreter::{env::global_env, eval::eval_module, value::Value},
    parser::parser::parse_path_all,
    stdlib::init::stdlib,
    type_checker::{checker::TypeChecker, types::GetType},
};

use crate::error::{print_parse_error, print_runtime_error, print_type_error};

/// Interpret a file and exit if any errors occurred.
pub fn handle_command_file(file: &str) {
    let std = stdlib();
    let module = match parse_path_all(Path::new(file), Some(&std)) {
        Ok(module) => module,
        Err(err) => {
            print_parse_error(err.message);
            exit(1);
        }
    };

    let mut checker = TypeChecker::default();
    std.init_type_checker(&mut checker);
    let mut env = global_env();
    std.init_environment(&mut env);
    let checked_module = match checker.check_module(&module) {
        Ok(module) => module,
        Err(err) => {
            print_type_error(err.message);
            exit(1);
        }
    };
    match eval_module(&checked_module, &mut env) {
        Ok(val) => {
            if val != Value::Unit {
                println!("{} {}", "Result:".light_green(), val);
                println!("{} {}", "Type:".light_green(), val.get_type());
            }
        }
        Err(err) => {
            print_runtime_error(err.message);
            exit(1);
        }
    };
    exit(0)
}
