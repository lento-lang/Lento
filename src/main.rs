mod args;

use colorful::{Colorful};
use args::lento_args;
use lento_core::LANG_VERSION;

fn _help() {
    const CLI_VERSION: &str = env!("CARGO_PKG_VERSION");
    println!("
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
    -l, --lint [files]              Lints the given files.
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
        VL = "|".dark_gray(),
        CLI_TITLE = "Lento CLI".bold(),
        CLI_VERSION = CLI_VERSION.yellow(),
        LANG_TITLE = "Lento lang".bold(),
        LANG_VERSION = LANG_VERSION.yellow(),
        LINK = "https://lento-lang.org".underlined().light_blue(),
        USAGE = "Usage".cyan(),
        CMD = "lt".bold(),
        ARGS = "(options) (files)".dim(),
        OPTIONS = "Options".cyan().underlined(),
        COMPILE_TARGETS = "Compile targets".cyan().underlined(),
        EXAMPLES = "Examples".cyan().underlined(),
        COPY = "Lento is free and open source software under the MIT license.\nCopyright ©️ 2021 William Rågstad, the Lento team and contributors.".dark_gray()
    );
}

fn main() {
    // _help();

    let mut arg_parser = lento_args();
    let args = arg_parser.get_matches_mut();
    if args.args_present() {
        println!("{:#?}", args);
    } else {
        arg_parser.print_help().unwrap();
    }
}
