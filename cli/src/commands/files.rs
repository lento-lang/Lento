use std::{path::Path, process::exit};

use clap::{parser::ValuesRef, Command};
use colorful::Colorful;

use lento_core::{
    interpreter::{
        environment::global_env,
        error::{runtime_error, RuntimeError},
        interpreter::interpret_module,
    },
    parser::{ast::Module, parser::parse_path_all},
    stdlib::init::stdlib,
    type_checker::{
        checker::TypeChecker,
        types::{std_types, GetType, TypeTrait},
    },
    util::failable::Failable,
};

use rayon::prelude::*;

use crate::error::{print_error, print_error_usage};

/**
 * Check that all input files exists.
 * If the first file doesn't have a file extension and also does not exist, throw an error for invalid subcommand.
 * Will exit the program if any error occurred.
 */
fn validate_files(files: &Vec<&Path>, arg_parser: &mut Command) {
    let mut is_first = true;
    for f in files {
        let has_ext = f.extension().is_some();
        if f.exists() {
            is_first = false;
            continue;
        } else if is_first && !has_ext {
            print_error_usage(
                format!(
                    "invalid command '{}' provided",
                    f.to_str().unwrap().yellow()
                ),
                arg_parser,
            );
        } else {
            print_error_usage(
                format!(
                    "{}{}{}",
                    "input file '",
                    f.to_str().unwrap().underlined(),
                    "' does not exist!"
                ),
                arg_parser,
            );
        }
        exit(1);
    }
}

fn parse_files<'a>(files: &Vec<&'a Path>) -> Vec<(&'a Path, Module)> {
    // Parallelize this parse-map operation to optimize detecting errors in multiple files (pre-execution)

    let std = stdlib();
    let results: Vec<(&'a Path, Module)> = files
        .par_iter()
        .filter_map(|f| match parse_path_all(f, Some(&std)) {
            Ok(module) => Some((*f, module)),
            Err(err) => {
                print_error(format!(
                    "failed to parse '{}': {}",
                    f.display(),
                    err.message
                ));
                None
            }
        })
        .collect();
    if results.len() < files.len() {
        print_error("One or more errors occurred during parsing!".to_string());
        exit(1);
    }
    results
}

/**
 * Interpret all files in order and exit the program if any runtime error occurred.
 */
fn interpret_parse_results(parse_results: Vec<(&Path, Module)>) -> Failable<Vec<RuntimeError>> {
    // Interpret all files in order. Unwrap is safe because we already checked for errors in the parse_results function
    let mut errors: Vec<RuntimeError> = vec![];
    let std = stdlib();
    let mut checker = TypeChecker::default();
    std.init_type_checker(&mut checker);
    let mut env = global_env();
    std.init_environment(&mut env);
    for (_, module) in parse_results {
        let checked_module = match checker.check_module(&module) {
            Ok(module) => module,
            Err(err) => {
                errors.push(runtime_error(err.message));
                continue;
            }
        };
        match interpret_module(&checked_module, &mut env) {
            Ok(val) => {
                if !val.get_type().equals(&std_types::UNIT) {
                    println!("{} {}", "Result:".light_green(), val);
                    println!("{} {}", "Type:".light_green(), val.get_type());
                }
            }
            Err(err) => errors.push(err),
        };
    }
    if !errors.is_empty() {
        return Err(errors);
    }
    Ok(())
}

pub fn handle_command_files(args: ValuesRef<String>, arg_parser: &mut Command) {
    let files = args.map(Path::new).collect();
    validate_files(&files, arg_parser);
    if let Err(err) = interpret_parse_results(parse_files(&files)) {
        // use print_error for each error
        for e in err {
            print_error(e.message);
        }
        exit(1);
    }
    exit(0)
}
