use clap::Command;
use colorful::Colorful;
use lento_core::{
    interpreter::error::RuntimeError,
    lexer::{lexer::InputSource, token::LineInfo},
    parser::error::ParseError,
    type_checker::checker::TypeError,
};

pub fn print_parse_error(err: ParseError, source: &InputSource) {
    print_error_kind("parse error", err.message);
    print_error_at(err.info, source)
}

pub fn print_runtime_error(err: RuntimeError, source: &InputSource) {
    print_error_kind("runtime error", err.message);
    print_error_at(err.info, source)
}

pub fn print_type_error(err: TypeError, source: &InputSource) {
    print_error_kind("type error", err.message);
    print_error_at(err.info, source)
}

pub fn print_error_kind(kind: &str, msg: String) {
    println!("{}: {}", kind.light_red(), msg);
}

pub fn print_error_at(info: LineInfo, source: &InputSource) {
    let msg = match info.end.eof {
        true => format!("line {}:{} to end of", info.start.line, info.start.column),
        false if info.start.line == info.end.line => format!(
            "line {}:{} to {}",
            info.start.line, info.start.column, info.end.column
        ),
        false => format!(
            "line {}:{} to line {}:{} in",
            info.start.line, info.start.column, info.end.line, info.end.column
        ),
    };
    print_error_kind("└─ at", format!("{} {}\n", msg, source));
}

pub fn print_error(msg: String) {
    print_error_kind("error", msg)
}

pub fn print_error_usage(msg: String, arg_parser: &mut Command) {
    print_error(msg);
    println!("{}\n", arg_parser.render_usage());
    println!("For more information try '{}'", "--help".bold())
}
