use std::io::Write;

use clap::{ArgMatches, Command};
use lento_core::{interpreter::{environment::Environment, interpreter::interpret_ast, value::Value}, util::str::Str, lexer::{readers::{stdin::StdinReader}, lexer::Lexer}, parser::parser::Parser, stdlib::init::init_environment};

use crate::error::print_error;

pub fn handle_command_repl(_args: &ArgMatches, _arg_parser: &mut Command) {
    let mut parser = Parser::new(Lexer::new(StdinReader::new(std::io::stdin())));
    let mut global_env = Environment::new(Str::from("global"));
    if let Err(err) = init_environment(&mut global_env) { print_error(err.message); return; }
    loop {
        print!("> "); std::io::stdout().flush().unwrap();
        match parser.parse() {
            Ok(ast) => match interpret_ast(&ast, &global_env) {
                Ok(value) => {
                    if value != Value::Unit { println!("{}", value); }
                },
                Err(err) => print_error(err.message),
            },
            Err(err) => print_error(err.message),
        }
        parser.get_lexer().get_reader().reset_reader();
        parser.get_lexer().reset();
    }
}
