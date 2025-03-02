use core::str;
use std::io::{Read, Write};

use clap::{ArgMatches, Command};
use colorful::Colorful;
use lento_core::{
    interpreter::env::global_env, lexer::lexer::InputSource, parser::parser, stdlib::init::stdlib,
    type_checker::checker::TypeChecker,
};

use crate::{commands::eval::eval_all, logger::init_logger_str, CLI_VERSION};

pub fn handle_command_repl(args: &ArgMatches, _arg_parser: &mut Command) {
    // Set the Ctrl-C handler to exit the program
    ctrlc::set_handler(|| std::process::exit(0)).expect("Error setting Ctrl-C handler");

    // Get flags
    let print_types = args.get_flag("types");
    if let Some(debug_level) = args.get_one::<String>("debug") {
        init_logger_str(debug_level);
    }

    // Print the version of the CLI and the language
    println!(
        "{CLI_TITLE} {V}{CLI_VERSION} {LANG_TITLE} {V}{LANG_VERSION}\n{SUBTEXT}",
        CLI_TITLE = "Lento cli".bold(),
        V = "v".yellow(),
        CLI_VERSION = CLI_VERSION.yellow(),
        LANG_TITLE = "language".bold(),
        LANG_VERSION = lento_core::LANG_VERSION.yellow(),
        SUBTEXT = "Interactive REPL, Ctrl+C to exit".dark_gray()
    );

    // Load the standard library initializer
    let std = stdlib();
    let source = InputSource::StdIn;

    // Create a parser that reads from stdin
    let mut parser = parser::from_stream(StdinLinesReader::default());
    std.init_parser(&mut parser);
    // Force the parsing to stop after the first EOF token
    // and not try to read more tokens from the reader,
    // this prevents the parser to infinitely wait for more input.
    parser.get_lexer().set_try_read_after_eof(false);
    // Force the lexer to read only once from the reader,
    // this prevents another prompt appearing after the
    // user has entered an expression.
    parser.get_lexer().set_read_only_once(true);
    let mut checker = TypeChecker::default();
    std.init_type_checker(&mut checker);
    let mut env = global_env();
    std.init_environment(&mut env);
    loop {
        print!("> ");
        std::io::stdout().flush().unwrap();
        let found_expr = eval_all(
            &mut parser,
            &mut checker,
            &mut env,
            print_types,
            true,
            &source,
        );
        let found_comment = str::from_utf8(parser.get_content())
            .unwrap_or_default()
            .trim()
            .starts_with("//");
        // Instead of creating a new parser, lexer, and reader, we simply reset them to save memory
        parser.get_lexer().reset();

        if !found_expr && !found_comment {
            // move the cursor up one line and clear it
            println!("\x1B[1A\x1B[K             	  ");
        }
    }
}

/// A reader that reads lines from stdin.
/// A whole line is read at a time and then returned in chunks for the parser to consume
/// all expressions together until no more is left.
#[derive(Default)]
struct StdinLinesReader {
    buffer: String,
}

impl Read for StdinLinesReader {
    fn read(&mut self, buf: &mut [u8]) -> std::io::Result<usize> {
        if self.buffer.is_empty() {
            std::io::stdin().read_line(&mut self.buffer)?;
            self.buffer = self.buffer.trim().to_string();
        }
        let bytes = self.buffer.as_bytes();
        let len = buf.len().min(bytes.len());
        buf[..len].copy_from_slice(&bytes[..len]);
        self.buffer = self.buffer[len..].to_string();
        Ok(len)
    }
}
