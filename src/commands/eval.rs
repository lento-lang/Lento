use std::io::Read;

use clap::{ArgMatches, Command};
use colorful::Colorful;
use lento_core::{
    interpreter::{
        environment::{global_env, Environment},
        interpreter::interpret_ast,
        value::Value,
    },
    parser::parser::{from_string, Parser},
    type_checker::{checker::TypeChecker, types::GetType},
};

use crate::{
    error::{print_parse_error, print_runtime_error, print_type_error},
    logger::init_logger_str,
};

pub fn handle_command_eval(args: &ArgMatches, _arg_parser: &mut Command) {
    // Get the flag for REPL
    if let Some(debug_level) = args.get_one::<String>("debug") {
        init_logger_str(debug_level);
    }
    let expr = args.get_one::<String>("expr").unwrap().to_owned();

    let mut parser = from_string(expr);
    let mut checker = TypeChecker::default();
    let mut env = global_env();
    eval_all(&mut parser, &mut checker, &mut env, false, false);
}

pub fn eval_all<R: Read>(
    parser: &mut Parser<R>,
    checker: &mut TypeChecker,
    env: &mut Environment,
    print_types: bool,
    colors: bool,
) {
    match parser.parse_all() {
        Ok(asts) => {
            'exprs: for (i, ast) in asts.iter().enumerate() {
                let checked_ast = match checker.check_expr(ast) {
                    Ok(ast) => ast,
                    Err(err) => {
                        print_type_error(err.message);
                        break 'exprs; // Stop on error
                    }
                };
                match interpret_ast(&checked_ast, env) {
                    Ok(value) => {
                        if i == asts.len() - 1 && value != Value::Unit {
                            if colors {
                                println!("{}", value.pretty_print_color());
                            } else {
                                println!("{}", value.pretty_print());
                            }
                            if print_types {
                                if colors {
                                    println!(
                                        "{} {}",
                                        "type:".dark_gray(),
                                        value.get_type().pretty_print_color()
                                    );
                                } else {
                                    println!("{} {}", "type:".dark_gray(), value.get_type());
                                }
                            }
                        }
                    }
                    Err(err) => {
                        print_runtime_error(err.message);
                        break 'exprs; // Stop on error
                    }
                }
            }
        }
        Err(err) => print_parse_error(err.message),
    }
}
