use clap::{ArgMatches, Command};
use colorful::Colorful;
use lento_core::{
    interpreter::{environment::global_env, interpreter::interpret_ast, value::Value},
    parser::parser::from_string,
    type_checker::types::GetType,
};

use crate::error::print_error;

pub fn handle_command_eval(args: &ArgMatches, _arg_parser: &mut Command) {
    let expr = args.get_one::<String>("expr").unwrap();
    let mut parser = from_string(expr);
    let env = global_env();
    match parser.parse() {
        Ok(ast) => match interpret_ast(&ast, &env) {
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
        },
        Err(err) => print_error(err.message),
    }
}
