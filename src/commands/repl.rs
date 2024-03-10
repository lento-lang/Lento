use std::io::Write;

use clap::{ArgMatches, Command};
use colorful::Colorful;
use lento_core::{
    interpreter::{environment::global_env, interpreter::interpret_ast, value::Value},
    parser::parser::from_stdin,
    type_checker::types::GetType,
};

use crate::error::print_error;

pub fn handle_command_repl(_args: &ArgMatches, _arg_parser: &mut Command) {
    let mut parser = from_stdin();
    let mut env = global_env();
    loop {
        print!("> ");
        std::io::stdout().flush().unwrap();
        match parser.parse_one() {
            Ok(ast) => match interpret_ast(&ast, &mut env) {
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
        // Instead of creating a new parser, lexer, and reader, we simply reset them to save memory
        parser.get_lexer().get_reader().reset_reader();
        parser.get_lexer().reset();
    }
}
