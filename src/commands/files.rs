
use std::{path::Path, process::exit};

use clap::{Command, parser::ValuesRef};
use colorful::{Colorful, core::StrMarker};
use lento_core::{parse_file, interpret_file, ParseSuccess, ParseFail};

use rayon::prelude::*;

use crate::error::{error, error_usage};

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

pub fn handle_command_files(args: ValuesRef<String>, arg_parser: &mut Command) {
    let files = args.map(Path::new).collect();
    validate_files(&files, arg_parser);
    interpret_parsed_files(parse_files(&files));
}