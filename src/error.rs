use clap::Command;
use colorful::Colorful;


pub fn error(msg: String) {
    println!("{}: {}\n", "error".light_red(), msg);
}

pub fn error_usage(msg: String, arg_parser: &mut Command) {
    error(msg);
    println!("{}\n", arg_parser.render_usage());
    println!("For more information try '{}'", "--help".bold())
}
