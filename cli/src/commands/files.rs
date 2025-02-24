use std::{path::Path, process::exit};

use colorful::Colorful;

use lento_core::{
    interpreter::{env::global_env, eval::eval_module, value::Value},
    lexer::lexer::InputSource,
    parser::parser::parse_path_all,
    stdlib::init::stdlib,
    type_checker::{checker::TypeChecker, types::GetType},
};

use crate::error::{print_parse_error, print_runtime_error, print_type_error};

/// Interpret a file and exit if any errors occurred.
pub fn handle_command_file(file: &str) {
    let file = Path::new(file);
    let source = InputSource::File(file.to_path_buf());
    let std = stdlib();
    let module = match parse_path_all(Path::new(file), Some(&std)) {
        Ok(module) => module,
        Err(err) => {
            print_parse_error(err, &source);
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
            print_type_error(err, &source);
            exit(1);
        }
    };
    match eval_module(&checked_module, &mut env) {
        Ok(val) => {
            if val != Value::Unit {
                println!("{} {}", "Result:".light_green(), val.pretty_print_color());
                println!(
                    "{} {}",
                    "Type:".light_green(),
                    val.get_type().pretty_print_color()
                );
            }
        }
        Err(err) => {
            print_runtime_error(err, &source);
            exit(1);
        }
    };
    exit(0)
}
