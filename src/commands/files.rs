use std::{path::Path, process::exit};

use clap::{Command, parser::ValuesRef};
use colorful::{Colorful, core::StrMarker};
use lento_core::parser::ast::Ast;
use lento_core::parser::parser::{ParseFail, ParserInput, parse_from_path};
use lento_core::interpreter::interpreter::interpret_ast;

use rayon::prelude::*;

use crate::error::{print_error, print_error_usage};

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

fn parse_files<'a>(files: &Vec<&'a Path>) -> Vec<(&'a Path, Result<Ast, ParseFail>)> {
    // Parallelize this parse-map operation to optimize detecting errors in multiple files (pre-execution)
    let parse_results: Vec<(&Path, Result<_, _>)> = files.par_iter()
        .map(|f| (*f, parse_from_path(*f)))
        .collect();
    let mut errors = false;
    for (file_path, parse_result) in &parse_results {
        match parse_result {
            Ok(result) => {
                match result.as_ref() {
                    Err(fail) => {
                        error(format!("failed to parse '{}': {}", file_path.display(), fail.msg));
                        errors = true;
                    },
                    _ => ()
                }
            },
            Err(err) => {
                error(format!("failed to parse '{}': {}", file_path.display(), err.to_string()));
                errors = true;
            }
        }
    }
    if errors {
        error("One or more errors occured during parsing!".to_str());
        exit(1);
    }
    return parse_results.into_iter().map(|(p, r)| (p, r.unwrap())).collect();
}

/**
 * Interpret all files in order and exit the program if any runtime error occured.
 */
fn interpret_parse_results<'a>(parse_results: Vec<(&'a Path, Result<Ast, ParseFail>)>) {
    // Interpret all files in order. Unwrap is safe because we already checked for errors in the parse_results function
    for (file_path, parse_result) in parse_results {
        let result_root = parse_result.unwrap();
        println!("{} '{}'...", "Interpreting".light_cyan(), file_path.display());
        match interpret_ast(&result_root) {
            Ok(()) => println!("{} executed program!", "Successfully".light_green()),
            Err((code, msg)) => error(format!("program exited with error code: {}. message: {}", code, msg))
        }
    }
}

pub fn handle_command_files(args: ValuesRef<String>, arg_parser: &mut Command) {
    let files = args.map(Path::new).collect();
    validate_files(&files, arg_parser);
    interpret_parse_results(parse_files(&files));
}
