use core::str;
use std::{fs::File, path::Path, process::exit};

use colorful::Colorful;

use lento_core::{
    interpreter::{env::global_env, eval::eval_exprs, value::Value},
    lexer::lexer::InputSource,
    parser::parser::from_file,
    stdlib::init::stdlib,
    type_checker::{checker::TypeChecker, types::GetType},
};

use crate::error::{print_error, print_parse_error, print_runtime_error, print_type_error};

/// Interpret a file and exit if any errors occurred.
pub fn handle_command_file(file: &str) {
    let file = Path::new(file);
    let source = InputSource::File(file.to_path_buf());
    let std = stdlib();
    let Ok(file) = File::open(file) else {
        print_error(format!(
            "Could not open file {}",
            file.display().to_string().light_red()
        ));
        exit(1);
    };
    let mut parser = from_file(file);
    std.init_parser(&mut parser);
    let exprs = match parser.parse_all() {
        Ok(exprs) => exprs,
        Err(err) => {
            print_parse_error(err, str::from_utf8(parser.get_content()).unwrap(), &source);
            exit(1);
        }
    };

    let mut checker = TypeChecker::default();
    std.init_type_checker(&mut checker);
    let mut env = global_env();
    std.init_environment(&mut env);
    let checked_exprs = match checker.check_top_exprs(&exprs) {
        Ok(exprs) => exprs,
        Err(err) => {
            print_type_error(err, str::from_utf8(parser.get_content()).unwrap(), &source);
            exit(1);
        }
    };
    match eval_exprs(&checked_exprs, &mut env) {
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
            print_runtime_error(err, str::from_utf8(parser.get_content()).unwrap(), &source);
            exit(1);
        }
    };
    exit(0)
}
