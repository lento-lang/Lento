#![allow(clippy::module_inception)]

pub mod compiler;
pub mod doc;
pub mod interpreter;
pub mod lexer;
pub mod parser;
pub mod printer;
pub mod project;
pub mod stdlib;
pub mod type_checker;
pub mod util;

pub const LANG_VERSION: &str = env!("CARGO_PKG_VERSION");
