mod conf;
mod args;

use args::{lento_args, lento_command};
use colorful::Colorful;

fn main() {
    let mut arg_parser = lento_args()
        .allow_external_subcommands(true);
    match arg_parser.get_matches_mut().subcommand() {
        Some((lento_command::BUILD, _))     => println!("build command"),
        Some((lento_command::COMPILE, _))   => println!("compile command"),
        Some((lento_command::DOC, _))       => println!("doc command"),
        Some((lento_command::EVAL, _))      => println!("eval command"),
        Some((lento_command::FMT, _))       => println!("fmt command"),
        Some((lento_command::LINT, _))      => println!("lint command"),
        Some((lento_command::REPL, _))      => println!("repl command"),
        Some((lento_command::RUN, _))       => println!("run command"),
        Some((lento_command::NEW, _))       => println!("new command"),
        Some((lento_command::TEST, _))      => println!("test command"),
        Some((invalid, _)) => {
            println!("{} {} {}\n", "Invalid command".red(), invalid, "provided".red());
            println!("{}", arg_parser.render_usage());
        },
        None => {
            println!("{}\n", "No command provided".red());
            println!("{}", arg_parser.render_usage());
        }
    }
}
