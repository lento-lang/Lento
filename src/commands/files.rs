use std::{path::Path, process::exit};

use lento_core::{util::{str::Str, failable::Failable}, stdlib::init::init_environment, interpreter::error::{RuntimeError, runtime_error}};
use clap::{Command, parser::ValuesRef};
use colorful::Colorful;
use lento_core::interpreter::environment::Environment;
use lento_core::interpreter::value::Value;
use lento_core::parser::ast::Ast;
use lento_core::parser::parser::parse_from_path;
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
        else if is_first && !has_ext {
            print_error_usage(format!("invalid command '{}' provided", f.to_str().unwrap().yellow()), arg_parser);
        } else {
            print_error_usage(format!("{}{}{}", "input file '", f.to_str().unwrap().underlined(), "' does not exist!"), arg_parser);
        }
        exit(1);
    }
}

fn parse_files<'a>(files: &Vec<&'a Path>) -> Vec<(&'a Path, Ast)> {
    // Parallelize this parse-map operation to optimize detecting errors in multiple files (pre-execution)
    let parse_results: Vec<(&Path, Result<_, _>)> = files.par_iter()
        .map(|f| (*f, parse_from_path(*f)))
        .collect();
    let mut errors = false;
    let mut parse_fail = |path: &Path, msg: &String| {
        print_error(format!("failed to parse '{}': {}", path.display(), msg));
        errors = true;
    };
    let mut results = vec![];
    for (file_path, parse_result) in &parse_results {
        match parse_result {
            Ok(result) => match result.as_ref() {
                Ok(ast) => results.push((*file_path, ast.to_owned())),
                Err(fail) => parse_fail(file_path, &fail.msg),
            },
            Err(err) => parse_fail(file_path, &err.to_string())
        }
    }
    if errors {
        print_error("One or more errors occured during parsing!".to_string());
        exit(1);
    }
    return results;
}

/**
 * Interpret all files in order and exit the program if any runtime error occured.
 */
fn interpret_parse_results<'a>(parse_results: Vec<(&'a Path, Ast)>) -> Failable<Vec<RuntimeError>> {
    // Interpret all files in order. Unwrap is safe because we already checked for errors in the parse_results function
    let mut errors: Vec<RuntimeError> = vec![];
    for (file_path, ast) in parse_results {
        println!("{} '{}'...", "Interpreting".light_cyan(), file_path.display());
        let mut global_env = Environment::new(Str::from("global"));
        if let Err(env_err) = init_environment(&mut global_env) { errors.push(env_err); }
        match interpret_ast(&ast, &global_env) {
            Ok(val) => {
                println!("{} executed program!", "Successfully".light_green());
                if val != Value::Unit {
                    println!("{} {}", "Result:".light_green(), val);
                }
            },
            Err(err) => errors.push(err)
        };
    }
    if errors.len() > 0 {
        errors.push(runtime_error("One or more errors occured during interpretation!".to_string()));
        return Err(errors);
    }
    Ok(())
}

pub fn handle_command_files(args: ValuesRef<String>, arg_parser: &mut Command) {
    let files = args.map(Path::new).collect();
    validate_files(&files, arg_parser);
    if let Err(err) = interpret_parse_results(parse_files(&files)) {
        // use print_error for each error
        for e in err { print_error(e.message); }
    } else {
        println!("{} interpreted all files!", "Successfully".light_green());
    }
}
