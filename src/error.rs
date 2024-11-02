use clap::Command;
use colorful::Colorful;

pub fn print_parse_error(msg: String) {
    println!("{}: {}\n", "parse error".light_red(), msg);
}

pub fn print_runtime_error(msg: String) {
    println!("{}: {}\n", "runtime error".light_red(), msg);
}

pub fn print_error(msg: String) {
    println!("{}: {}\n", "error".light_red(), msg);
}

pub fn print_error_usage(msg: String, arg_parser: &mut Command) {
    print_error(msg);
    println!("{}\n", arg_parser.render_usage());
    println!("For more information try '{}'", "--help".bold())
}
