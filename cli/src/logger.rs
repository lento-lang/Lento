// A custom logger that logs to stdout and stderr.
// Used when `--debug` is passed.

use env_logger::Builder;
use log::LevelFilter;
use std::io::Write;

use crate::error::print_error;

const BRIGHT_BLACK: &str = "\x1b[90m";
const RESET: &str = "\x1b[0m";

pub fn init_logger(level: LevelFilter) {
    Builder::new()
        .filter_level(level)
        .default_format()
        .format(|buf, record| {
            write!(buf, "{BRIGHT_BLACK}[{RESET}")?;
            write!(
                buf,
                "{style}{lvl}{style:#}",
                lvl = record.level(),
                style = buf.default_level_style(record.level()),
            )?;
            write!(
                buf,
                " {}:{}",
                record.target(),
                record.line().unwrap_or_default()
            )?;
            write!(buf, "{BRIGHT_BLACK}]{RESET}")?;
            writeln!(buf, " {}", record.args())
        })
        .target(env_logger::Target::Stdout)
        .init()
}

pub fn init_logger_str(level: &str) {
    let level = match level {
        "error" => LevelFilter::Error,
        "warn" => LevelFilter::Warn,
        "info" => LevelFilter::Info,
        "debug" => LevelFilter::Debug,
        "trace" => LevelFilter::Trace,
        _ => {
            print_error(format!(
                "Invalid log level '{}', expected one of 'trace', 'debug', 'info', 'warn' or 'error'",
                level
            ));
            std::process::exit(1);
        }
    };
    init_logger(level);
    log::info!("Setting log level to {}", level);
}
