#[derive(Debug, Clone, PartialEq, PartialOrd, Eq, Ord)]
pub struct LocationInfo {
    pub index: usize,
    pub line: usize,
    pub column: usize,
    pub eof: bool,
}

impl LocationInfo {
    pub fn new(index: usize, line: usize, column: usize) -> Self {
        Self {
            index,
            line,
            column,
            eof: false,
        }
    }

    pub fn eof(index: usize) -> Self {
        Self {
            index,
            line: 0,
            column: 0,
            eof: true,
        }
    }
}

impl Default for LocationInfo {
    fn default() -> Self {
        Self {
            index: 0,
            line: 1,
            column: 1,
            eof: false,
        }
    }
}

#[derive(Debug, Clone, Default, PartialEq, PartialOrd, Eq, Ord)]
pub struct LineInfo {
    pub start: LocationInfo,
    pub end: LocationInfo,
}

impl LineInfo {
    pub fn new(start: LocationInfo, end: LocationInfo) -> Self {
        Self { start, end }
    }

    pub fn eof(start: LocationInfo, eof_index: usize) -> Self {
        Self {
            start,
            end: LocationInfo::eof(eof_index),
        }
    }

    pub fn join(&self, other: &LineInfo) -> LineInfo {
        let start = if self.start.index < other.start.index {
            self.start.clone()
        } else {
            other.start.clone()
        };
        let end = if self.end.index > other.end.index {
            self.end.clone()
        } else {
            other.end.clone()
        };

        LineInfo { start, end }
    }
}

#[derive(Debug, Clone, PartialEq, PartialOrd, Eq, Ord)]
pub struct BaseErrorLabel {
    pub message: String,
    pub info: LineInfo,
}

/// A type error is an error that occurs during type checking.
#[derive(Debug, Clone, PartialEq, PartialOrd, Eq, Ord)]
pub struct BaseError {
    pub message: String,
    pub info: LineInfo,
    pub hint: Option<String>,
    pub labels: Vec<BaseErrorLabel>,
}

impl BaseError {
    pub fn new(message: String, info: LineInfo) -> Self {
        Self {
            message,
            info,
            hint: None,
            labels: Vec::new(),
        }
    }
    pub fn with_hint(self, hint: String) -> Self {
        Self {
            hint: Some(hint),
            ..self
        }
    }

    pub fn with_label(self, message: String, info: LineInfo) -> Self {
        let mut labels = self.labels;
        labels.push(BaseErrorLabel { message, info });
        Self { labels, ..self }
    }
}
