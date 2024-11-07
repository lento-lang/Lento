// A custom logger that logs to stdout and stderr.
// Used when `--debug` is passed.

use env_logger::Builder;
use log::LevelFilter;

use crate::error::print_error;

pub fn init_logger(level: LevelFilter) {
    Builder::new()
        .filter_level(level)
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
}
