use colorful::Colorful;

use crate::type_checker::types::Type;

use crate::util::error::{BaseError, BaseErrorExt, LineInfo};

#[derive(Debug, Clone)]
pub struct LexerError {
    inner: BaseError,
}

impl BaseErrorExt for LexerError {
    fn new(message: String, info: LineInfo) -> Self {
        Self {
            inner: BaseError::new(message, info),
        }
    }

    fn with_hint(self, hint: String) -> Self {
        Self {
            inner: self.inner.with_hint(hint),
        }
    }

    fn with_label(self, message: String, info: LineInfo) -> Self {
        Self {
            inner: self.inner.with_label(message, info),
        }
    }

    fn base(&self) -> &BaseError {
        &self.inner
    }

    fn to_base(self) -> BaseError {
        self.inner
    }
}

impl LexerError {
    pub fn is_eof_error(&self) -> bool {
        self.inner.info.end.eof
    }

    pub fn unexpected_end_of_file(info: LineInfo) -> Self {
        Self::new("Unexpected end of program".to_string(), info)
    }

    pub fn unexpected_character(c: char, info: LineInfo) -> Self {
        Self::new(
            format!("Unexpected character {}", c.to_string().yellow()),
            info,
        )
    }

    pub fn invalid_char(c: String, info: LineInfo) -> Self {
        Self::new(
            format!("Invalid character literal '{}'", c.to_string().yellow()),
            info,
        )
    }

    pub fn invalid_number_type(num: &str, ty: &Type, info: LineInfo) -> Self {
        Self::new(
            format!(
                "Invalid {} literal {}",
                ty.pretty_print_color(),
                num.yellow()
            ),
            info,
        )
    }

    pub fn invalid_number(num: &str, info: LineInfo) -> Self {
        Self::new(format!("Invalid number literal {}", num.yellow()), info)
    }
}
