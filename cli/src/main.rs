mod args;
mod commands;
mod error;
mod logger;

use std::process::exit;

use args::{lento_args, lento_command};

use commands::{eval::handle_command_eval, files::handle_command_file, repl::handle_command_repl};
use error::print_error_usage;
use logger::init_logger_str;

pub const CLI_VERSION: &str = env!("CARGO_PKG_VERSION");

fn main() {
    let mut arg_parser = lento_args();
    let args = arg_parser.get_matches_mut();
    if let Some(debug_level) = args.get_one::<String>("debug") {
        init_logger_str(debug_level);
    }

    match args.subcommand() {
        Some((lento_command::BUILD, _)) => println!("The build command is not yet implemented!"),
        Some((lento_command::COMPILE, _)) => {
            println!("The compile command is not yet implemented!")
        }
        Some((lento_command::DOC, _)) => println!("The doc command is not yet implemented!"),
        Some((lento_command::EVAL, args)) => handle_command_eval(args, &mut arg_parser),
        Some((lento_command::FMT, _)) => println!("The fmt command is not yet implemented!"),
        Some((lento_command::LINT, _)) => println!("The lint command is not yet implemented!"),
        Some((lento_command::REPL, args)) => handle_command_repl(args, &mut arg_parser),
        Some((lento_command::RUN, _)) => println!("The run command is not yet implemented!"),
        Some((lento_command::NEW, _)) => println!("The new command is not yet implemented!"),
        Some((lento_command::TEST, _)) => println!("The test command is not yet implemented!"),
        _ => match args.get_one::<String>("file") {
            Some(raw_file) => handle_command_file(raw_file),
            _ => {
                print_error_usage("No command provided".to_string(), &mut arg_parser);
                println!("{:#?}", args);
                exit(1);
            }
        },
    }
}
