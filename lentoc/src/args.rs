use chrono::Datelike;
use clap::{arg, Command};
use colorful::{Color, Colorful};

use lentoc_lib::LANG_VERSION;

use crate::CLI_VERSION;

const MASCOT: &str = r#"
       _...---..._
       '---._.---'
         () _ ()
      / \   V   /|
     /   '--.--' |
    /    .       /
   /     |     .'
  /    .'\  .-' \
 /__.-'   \__\\__\
"#;

pub mod lento_command {
    pub const EVAL: &str = "eval";
    pub const REPL: &str = "repl";
}

pub fn lento_args() -> Command {
    let title_short = format!("{CLI_TITLE} {V}{CLI_VERSION}, {LANG_TITLE} {V}{LANG_VERSION}\nA command line utility for the Lento toolchain.",
        CLI_TITLE = "Lento cli".bold(),
        V = "v".yellow(),
        CLI_VERSION = CLI_VERSION.yellow(),
		LANG_TITLE = "language".bold(),
        LANG_VERSION = LANG_VERSION.yellow());

    let title_long = format!(
        "\n\
{MASCOT}
{VL} {CLI_TITLE} version {CLI_VERSION} and {LANG_TITLE} version {LANG_VERSION}.
{VL} A command line utility for the Lento toolchain and ecosystem.
{VL} See {LINK} for more information.",
        MASCOT = MASCOT.color(Color::RoyalBlue1).bold(),
        VL = "|".dark_gray(),
        CLI_TITLE = "Lento cli".bold(),
        CLI_VERSION = CLI_VERSION.yellow(),
        LANG_TITLE = "language".bold(),
        LANG_VERSION = LANG_VERSION.yellow(),
        LINK = "https://lento-lang.org".underlined().light_blue()
    );

    let examples = format!(
        "{EXAMPLES}
  {LT} file1.lt file2.lt              Interpret file1.lt and file2.lt in order
  {LT} {EVAL} \"1 + 1\"                      Evaluate the expression 1 + 1
  {LT} {REPL}                              Start the REPL",
        EXAMPLES = "Examples:".bold().underlined(),
        LT = "lt".bold(),
        EVAL = "e".bold(),
        REPL = "r".bold()
    );

    // Current year
    let copy = format!("Lento is free and open source software under the MIT license.\nCopyright ©️2017-{:?} William Rågstad, the Lento team and contributors.\n",
		chrono::Local::now().year()).dark_gray();

    let debug_arg = arg!(-d --debug [level] "Turns on additional debugging information")
        .value_parser(["trace", "debug", "info", "warn", "error"])
        .default_missing_value("debug");

    Command::new("Lento CLI")
    .bin_name("lt")
    .before_help(title_short.clone())
    .before_long_help(title_long.clone())
	.arg(arg!([file] "Interprets the given file"))
	.arg(debug_arg.clone())
    // .term_width(80)
    .version(CLI_VERSION)
    .long_version(title_long)
    .help_template("{before-help}{usage-heading} {usage}\n\n{all-args}{after-help}")
    .override_usage(format!("{} {}", "lt".bold(), "[command] (options) (file)".dim()))
    // .next_help_heading("\x1B[38;5;6mOptions\x1B[0m:\x1B[8m")
    // .args([
    //     arg!([file] "Interprets the given file in order").help_heading("Interpreter")
    // ])
    // .subcommand_help_heading("\x1B[38;5;6mCommands\x1B[0m: ")
    .subcommand(
        Command::new(lento_command::EVAL)
        .alias("e")
        .about("Evaluate an expression")
        .long_about("Evaluates an expression and prints the result.\nThis feature is useful for quick testing and debugging.\nUse the REPL for interactive development.")
        .version("1.0")
        .override_usage(format!("{} {}", "lt eval".bold(), "(options) [expr]".dim()))
        .args([
            arg!(<expr> "Sets the expression to evaluate"),
            debug_arg.clone(),
        ])
    )
    .subcommand(
        Command::new(lento_command::REPL)
        .alias("r")
        .about("Start the interactive REPL")
        .long_about("Starts the REPL, which is an interactive development environment.\nUse this command to quickly test and debug your code.")
        .version("1.0")
        .override_usage(format!("{} {}", "lt repl".bold(), "(options)".dim()))
		.args([
			arg!(-t --types "Print the types of values"),
            debug_arg.clone(),
		])
    )
    .after_long_help(format!("{examples}\n\n{copy}"))
    .arg_required_else_help(true)
}

// Previous help design
pub fn _help() {
    println!("
{MASCOT}
{VL} {CLI_TITLE} version {CLI_VERSION}.
{VL} {LANG_TITLE} version {LANG_VERSION}.
{VL} A command line interface tool for the Lento programming language.
{VL} See {LINK} for more information.

{USAGE}: {CMD} {ARGS}

{OPTIONS}:
    -h, --help                      Prints this help message.
    -v, --version                   Prints the version of the program.
    -e, --evaluate [expr]           Evaluate one or more expressions.
    -r, --repl (verbose)            Starts the REPL mode.
    -l, --lint [file]               Lints the given file.
    -c, --compile (target) [file]   Compiles the given file. (Not implemented)

{COMPILE_TARGETS}:                    Cross compile to a target language or platform.
    js                              JavaScript (Web)
    node                            JavaScript (Node.js)
    llvm                            LLVM IR assembly
    asm                             x86 assembly
    dll                             Dynamically linked library
    exe                             Standalone executable

{EXAMPLES}:
    lt file1.lt file2.lt            Interpret file1.lt and file2.lt in order.
    lt -e \"1 + 1\"                   Evaluate the expression 1 + 1.
    lt -r                           Start the REPL.
    lt -c file1.lt                  Compile file1.lt to a standalone executable.
    lt -c js file1.lt               Cross compile file1.lt to JavaScript.
    lt -c asm file1.lt              Cross compile file1.lt to x86 assembly.

{COPY}
    ",
        MASCOT = MASCOT.color(Color::RoyalBlue1).bold(),
        VL = "|".dark_gray(),
        CLI_TITLE = "Lento CLI".bold(),
        CLI_VERSION = CLI_VERSION.yellow(),
        LANG_TITLE = "Lento lang".bold(),
        LANG_VERSION = LANG_VERSION.yellow(),
        LINK = "https://lento-lang.org".underlined().light_blue(),
        USAGE = "Usage".cyan(),
        CMD = "lt".bold(),
        ARGS = "(options) (file)".dim(),
        OPTIONS = "Options".cyan().underlined(),
        COMPILE_TARGETS = "Compile targets".cyan().underlined(),
        EXAMPLES = "Examples".cyan().underlined(),
        COPY = format!("Lento is free and open source software under the MIT license.\nCopyright ©️{:?} William Rågstad, the Lento team and contributors.\n",
			chrono::Local::now().year()).dark_gray()
    );
}
