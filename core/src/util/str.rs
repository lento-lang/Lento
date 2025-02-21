use std::fmt::Display;

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Str {
    String(String),
    Str(&'static str),
}

impl Str {
    pub fn empty() -> Self {
        Str::Str("")
    }
}

impl Display for Str {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Str::String(s) => s.fmt(f),
            Str::Str(s) => s.fmt(f),
        }
    }
}

impl From<String> for Str {
    fn from(s: String) -> Self {
        Str::String(s)
    }
}

impl From<&'static str> for Str {
    fn from(s: &'static str) -> Self {
        Str::Str(s)
    }
}
