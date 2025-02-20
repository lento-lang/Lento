use std::fmt::Display;

#[derive(Clone, Debug, Eq, PartialOrd, Ord)]
pub enum Str {
    String(String),
    Str(&'static str),
}

impl PartialEq for Str {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Str::String(s1), Str::String(s2)) => s1 == s2,
            (Str::Str(s1), Str::Str(s2)) => s1 == s2,
            (Str::String(s1), Str::Str(s2)) => s1 == s2,
            (Str::Str(s1), Str::String(s2)) => s1 == s2,
        }
    }
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
