use clap::{ArgMatches, Command};
use colorful::Colorful;
use lento_core::{
    interpreter::{environment::global_env, interpreter::interpret_ast, value::Value},
    parser::parser::from_string,
    type_checker::{checker::TypeChecker, types::GetType},
};

use crate::error::{print_error, print_type_error};

pub fn handle_command_eval(args: &ArgMatches, _arg_parser: &mut Command) {
    let expr = args.get_one::<String>("expr").unwrap().to_owned();
    let mut parser = from_string(expr);
    let mut checker = TypeChecker::default();
    let mut env = global_env();
    match parser.parse_one() {
        Ok(ast) => {
            let checked_ast = match checker.check_expr(&ast) {
                Ok(ast) => ast,
                Err(err) => {
                    print_type_error(err.message);
                    return;
                }
            };
            match interpret_ast(&checked_ast, &mut env) {
                Ok(value) => {
                    if value != Value::Unit {
                        println!("{}", value);
                        println!(
                            "{}{}{}",
                            "(type: ".dark_gray(),
                            value.get_type().to_string().dark_gray(),
                            ")".dark_gray()
                        );
                    }
                }
                Err(err) => print_error(err.message),
            }
        }
        Err(err) => print_error(err.message),
    }
}
