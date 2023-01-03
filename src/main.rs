mod conf;
mod args;

use std::{path::Path, process::exit};

use args::{lento_args, lento_command};
use clap::{Command};
use colorful::{Colorful, core::StrMarker};
use lento_core::{parse_file, Ast, interpret_file, ParseSuccess, ParseFail};

use rayon::prelude::*;

fn error(msg: String) {
    println!("{}: {}\n", "error".light_red(), msg);
}

fn error_usage(msg: String, arg_parser: &mut Command) {
    error(msg);
    println!("{}\n", arg_parser.render_usage());
    println!("For more information try '{}'", "--help".bold())
}

/**
 * Check that all input files exists.
 * If the first file doesn't have a file extension and also does not exist, throw an error for invalid subcommand.
 * Will exit the program if any error occured.
 */
fn validate_files(files: &Vec<&Path>, arg_parser: &mut Command) {
    let mut is_first = true;
    for f in files {
        let has_ext = f.extension().is_some();
        if f.exists() { is_first = false; continue; }
        else if is_first && !has_ext { error_usage(format!("invalid command '{}' provided", f.to_str().unwrap().yellow()), arg_parser); }
        else { error_usage(format!("{}{}{}", "input file '", f.to_str().unwrap().underlined(), "' does not exist!"), arg_parser); }
        exit(1);
    }
}

fn parse_files<'a>(files: &'a Vec<&'a Path>) -> Vec<Result<ParseSuccess<'a>, ParseFail<'a>>> {
    // Parallelize this parse-map operation to optimize detecting errors in multiple files (pre-execution)
    let parse_results: Vec<Result<_, _>> = files.par_iter().map(|f| parse_file(*f)).collect();
    let mut errors = false;
    for parse_result in &parse_results {
        if let Err(fail) = parse_result {
            error(format!("failed to parse '{}': {}", fail.source_file.file_name().unwrap().to_str().unwrap(), fail.msg));
            errors = true;
        }
    }
    if errors {
        error("One or more errors occured during parsing!".to_str());
        exit(1);
    }
    return parse_results;
}

fn interpret_parsed_files<'a>(parse_results: Vec<Result<ParseSuccess<'a>, ParseFail<'a>>>) {
    // Interpret all files in order
    // Unwrap is safe because we already checked for errors in the parse_results function
    for parsed_file in parse_results {
        let root_ast_node = parsed_file.as_ref().unwrap();
        println!("{} '{}'...", "Interpreting".light_cyan(), root_ast_node.source_file.file_name().unwrap().to_str().unwrap());
        match interpret_file(root_ast_node.source_file) {
            Ok(()) => println!("{} executed program!", "Successfully".light_green()),
            Err((code, msg)) => error(format!("program exited with error code: {}. message: {}", code, msg))
        }
    }
}

fn main() {
    let mut arg_parser = lento_args();
    let args = arg_parser.get_matches_mut();
    match args.subcommand() {
        Some((lento_command::BUILD, _))     => println!("The build command is not yet implemented!"),
        Some((lento_command::COMPILE, _))   => println!("The compile command is not yet implemented!"),
        Some((lento_command::DOC, _))       => println!("The doc command is not yet implemented!"),
        Some((lento_command::EVAL, _))      => println!("The eval command is not yet implemented!"),
        Some((lento_command::FMT, _))       => println!("The fmt command is not yet implemented!"),
        Some((lento_command::LINT, _))      => println!("The lint command is not yet implemented!"),
        Some((lento_command::REPL, _))      => println!("The repl command is not yet implemented!"),
        Some((lento_command::RUN, _))       => println!("The run command is not yet implemented!"),
        Some((lento_command::NEW, _))       => println!("The new command is not yet implemented!"),
        Some((lento_command::TEST, _))      => println!("The test command is not yet implemented!"),
        _ => match args.get_many::<String>("files") {
            Some(raw_files) => {
                let files: Vec<&Path> = raw_files.map(Path::new).collect();
                validate_files(&files, &mut arg_parser);
                interpret_parsed_files(parse_files(&files));
            },
            _ => {
                error_usage("No command provided".to_string(), &mut arg_parser);
                println!("{:#?}", args);
                exit(1);
            }
        }
    }
}
