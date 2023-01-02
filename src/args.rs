use colorful::Colorful;
use clap::{Command, arg};

use lento_core::LANG_VERSION;

pub fn lento_args() -> Command {
    const CLI_VERSION: &str = env!("CARGO_PKG_VERSION");

    let title_short = format!("{CLI_TITLE} {V}{CLI_VERSION}\nA command line interface tool for the Lento programming language.",
        CLI_TITLE = "Lento CLI".bold(),
        V = "v".yellow(),
        CLI_VERSION = CLI_VERSION.yellow());

    let title_long = format!("\n\
{VL} {CLI_TITLE} version {CLI_VERSION} and {LANG_TITLE} version {LANG_VERSION}.
{VL} A command line interface tool for the Lento programming language.
{VL} See {LINK} for more information.",
        VL = "|".dark_gray(),
        CLI_TITLE = "Lento CLI".bold(),
        CLI_VERSION = CLI_VERSION.yellow(),
        LANG_TITLE = "Lento Lang".bold(),
        LANG_VERSION = LANG_VERSION.yellow(),
        LINK = "https://lento-lang.org".underlined().light_blue());

    let examples = format!("{EXAMPLES}
  {LT} {FILE1} {FILE2}              Interpret {FILE1} and {FILE2} in order
  {LT} {EVAL} \"1 + 1\"                   Evaluate the expression 1 + 1
  {LT} {REPL}                           Start the REPL
  {LT} {COMP} {FILE1}               Compile {FILE1} to a standalone executable
  {LT} {COMP} --target js {FILE1}   Cross compile {FILE1} to JavaScript
  {LT} {C} --target asm {FILE1}        Cross compile {FILE1} to x86 assembly",
        EXAMPLES = "Examples:".bold().underlined(),
        LT = "lt".bold(),
        FILE1 = "file1.lt".light_yellow(),
        FILE2 = "file2.lt".light_yellow(),
        EVAL = "eval".bold(),
        REPL = "repl".bold(),
        COMP = "compile".bold(),
        C = "c".bold());

    let compile_targets = format!("{COMPILE_TARG}
  {EXE}         Native standalone executable binary
  {DLL}         Dynamically linked library/shared object
  {ASM}         x86 assembly (Intel syntax/AT&T syntax)
  {LLVM}        LLVM IR assembly (text)
  {LLBC}     LLVM bitcode (LLVM IR binary)
  {JS}          JavaScript (Web)
  {NODE}        JavaScript (Node.js)
  {WASM}        WebAssembly",
        COMPILE_TARG = "Compile targets:".bold().underlined(),
        EXE = "exe".bold(),
        DLL = "dll".bold(),
        ASM = "asm".bold(),
        LLVM = "llvm".bold(),
        LLBC = "llvm-bc".bold(),
        JS = "js".bold(),
        NODE = "node".bold(),
        WASM = "wasm".bold());

    let doc_targets = format!("{DOC_TARG}
  {HTML}        HTML documentation
  {MD}          Markdown documentation
  {LAT}       LaTeX documentation
  {PDF}         PDF documentation
  {DOCX}        DOCX documentation",
        DOC_TARG = "Documentation targets:".bold().underlined(),
        HTML = "html".bold(),
        MD = "md".bold(),
        LAT = "latex".bold(),
        PDF = "pdf".bold(),
        DOCX = "docx".bold());

    let copy = "Lento is free and open source software under the MIT license.\nCopyright ©️ 2021 William Rågstad, the Lento team and contributors.\n".dark_gray().to_string();

    Command::new("Lento CLI")
    .bin_name("lt")
    .before_help(title_short.clone())
    .before_long_help(title_long.clone())
    // .term_width(80)
    .version(CLI_VERSION)
    .long_version(title_long)
    .help_template("{before-help}{usage-heading} {usage}\n\n{all-args}{after-help}")
    .override_usage(format!("{} {}", "lt".bold(), "[command] (options) (files)".dim()))
    // .next_help_heading("\x1B[38;5;6mOptions\x1B[0m:\x1B[8m")
    .args([
        arg!([files] "Interprets the given files in order").help_heading("Interpreter")
    ])
    // .subcommand_help_heading("\x1B[38;5;6mCommands\x1B[0m: ")
    .subcommand(
        Command::new("build")
        .alias("b")
        .about("Build project")
        .long_about("Builds a project to a standalone executable, a dynamically linked library,\nor cross compile to a target language or platform.")
        .version("1.0")
        .override_usage(format!("{} {}", "lt build".bold(), "(options)".dim()))
        .args([
            arg!(-r --release "Builds the project in release mode"),
            arg!(-t --target <target> "Overrides the target to build for specified in the project file"),
            arg!(-v --verbose "turns on verbose information"),
            arg!(-d --debug "Turns on additional debugging information"),
        ])
        .after_help(compile_targets.clone())
    )
    .subcommand(
        Command::new("compile")
        .alias("c")
        .about("Compile file")
        .long_about("Compiles a file to a standalone executable, a dynamically linked library,\nor cross compile to a target language or platform.")
        .version("1.0")
        .override_usage(format!("{} {}", "lt compile".bold(), "(options) [files]".dim()))
        .args([
            arg!(<files> "Sets the input file(s) to use"),
            arg!(-t --target <target> "Sets the target to compile for"),
            arg!(-o --output <output> "Sets the output file to use").value_name("testing"),
            arg!(-v --verbose "turns on verbose information"),
            arg!(-d --debug "Turns on additional debugging information"),
        ])
        .after_help(compile_targets.clone())
    )
    .subcommand(
        Command::new("doc")
        .alias("d")
        .about("Generate documentation")
        .long_about("Generates documentation for a project or a file.\nSupports Markdown, HTML, and LaTeX.")
        .version("1.0")
        .override_usage(format!("{} {}", "lt doc".bold(), "(options) (files)".dim()))
        .args([
            arg!([files] "Sets the input files to use (default: all files in the current directory)"),
            arg!(-o --output <output> "Sets the output file to use"),
            arg!(-t --target <target> "Sets the target to generate documentation for"),
            arg!(-v --verbose "turns on verbose information"),
            arg!(-d --debug "Turns on additional debugging information"),
        ])
        .after_help(doc_targets)
    )
    .subcommand(
        Command::new("eval")
        .alias("e")
        .about("Evaluate an expression")
        .long_about("Evaluates an expression and prints the result.\nThis feature is useful for quick testing and debugging.\nUse the REPL for interactive development.")
        .version("1.0")
        .override_usage(format!("{} {}", "lt eval".bold(), "(options) [expr]".dim()))
        .args([
            arg!(<expr> "Sets the expression to evaluate"),
            arg!(-d --debug "Turns on additional debugging information"),
        ])
    )
    .subcommand(
        Command::new("fmt")
        .alias("f")
        .about("Format a file or project")
        .long_about("Formats a file or a all files in the current project.\nThis command is useful for keeping code consistent and readable.")
        .version("1.0")
        .override_usage(format!("{} {}", "lt fmt".bold(), "(options) (files)".dim()))
        .args([
            arg!([files] "Sets the input files to use (default: all files in the current directory)"),
            arg!(-v --verbose "turns on verbose information"),
            arg!(-d --debug "Turns on additional debugging information"),
        ])
    )
    .subcommand(
        Command::new("lint")
        .alias("l")
        .about("Lints one or more files")
        .long_about("Lints one or more files and prints any warnings or errors.\nPassing no files will lint all files in the current directory.")
        .version("1.0")
        .override_usage(format!("{} {}", "lt lint".bold(), "(options) (files)".dim()))
        .args([
            arg!([files] "Sets the input files to use (default: all files in the current directory)"),
            arg!(-v --verbose "turns on verbose information"),
            arg!(-d --debug "Turns on additional debugging information"),
        ])
    )
    .subcommand(
        Command::new("repl")
        .alias("r")
        .about("Start the interactive REPL")
        .long_about("Starts the REPL, which is an interactive development environment.\nUse this command to quickly test and debug your code.")
        .version("1.0")
        .override_usage(format!("{} {}", "lt repl".bold(), "(options)".dim()))
        .args([
            arg!(-d --debug "Turns on additional debugging information"),
        ])
    )
    .subcommand(
        Command::new("run")
        .about("Run project")
        .long_about("Runs a project in debug mode, which means that the program will be compiled and executed.\nUse this command for quick testing and debugging.")
        .version("1.0")
        .override_usage(format!("{} {}", "lt run".bold(), "(options)".dim()))
        .args([
            arg!(-v --verbose "turns on verbose information"),
            arg!(-d --debug "Turns on additional debugging information"),
        ])
    )
    .subcommand(
        Command::new("new")
        .alias("n")
        .about("Create a new project")
        .long_about("Creates a new project with the given name.\nUse the --template option to specify a template to use.\nUse the --license option to specify a license to use.\nUse the --author option to specify the author of the project.\nUse the --description option to specify a description of the project.\nUse the --git option to initialize a git repository.")
        .version("1.0")
        .override_usage(format!("{} {}", "lt new".bold(), "(options) [name]".dim()))
        .args([
            arg!([name] "Sets the name of the project").required(true),
            arg!(-t --template [template] "Sets the template to use"),
            arg!(-l --license [license] "Sets the license to use"),
            arg!(-a --author [author] "Sets the author to use"),
            arg!(-d --description [description] "Sets the description to use"),
            arg!(-g --git "Initializes a git repository"),
            arg!(-v --verbose "turns on verbose information"),
        ])
    )
    .subcommand(
        Command::new("test")
        .alias("t")
        .about("Run unit tests")
        .long_about("Runs unit tests in the current project.\nPassing no files will run all tests in the current directory.")
        .version("1.0")
        .override_usage(format!("{} {}", "lt test".bold(), "(options) (files)".dim()))
        .args([
            arg!([files] "Sets the input files to use (default: all files in the current directory)"),
            arg!(-v --verbose "turns on verbose information"),
            arg!(-d --debug "Turns on additional debugging information"),
        ])
    )
    .after_long_help(format!("{examples}\n\n{copy}"))

}

