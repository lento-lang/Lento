use std::{
    cell::{Cell, RefCell},
    collections::HashSet,
    fmt::Display,
    fs::File,
    io::{BufReader, Cursor, Error, ErrorKind, Read},
    path::PathBuf,
    str::FromStr,
};

use lazy_regex::regex_replace_all;
use malachite::{num::conversion::traits::FromSciString, Integer, Natural, Rational};

use crate::{
    interpreter::number::{BitSize, FloatingPoint, Number, SignedInteger, UnsignedInteger},
    type_checker::types::std_types,
};

use super::{
    error::LexerError,
    readers::{bytes_reader::BytesReader, stdin::StdinReader},
    token::{LineInfo, TokenInfo, TokenKind},
};

/// The source type of the program input. \
/// This is used to determine how to read the
/// input for the program.
/// And improve error messages.
#[derive(Debug, Clone, PartialEq)]
pub enum InputSource {
    /// A file path to the program source. \
    /// 1. The string is the path to the file.
    File(PathBuf),
    /// A static string containing the program source.
    String,
    /// A stream of characters. \
    /// 1. The string is the name of the stream.
    Stream(String),
    StdIn,
}

impl Display for InputSource {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            InputSource::File(path) => write!(f, "file '{}'", path.to_string_lossy()),
            InputSource::String => write!(f, "string"),
            InputSource::Stream(name) => write!(f, "stream '{}'", name),
            InputSource::StdIn => write!(f, "stdin"),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
struct NumInfo {
    pub float: bool,
    pub signed: bool,
    pub bits: BitSize,
}

impl NumInfo {
    pub fn uint(bits: BitSize) -> Self {
        Self {
            float: false,
            signed: false,
            bits,
        }
    }

    pub fn int(bits: BitSize) -> Self {
        Self {
            float: false,
            signed: true,
            bits,
        }
    }

    pub fn float(bits: BitSize) -> Self {
        Self {
            float: true,
            signed: true,
            bits,
        }
    }
}

//--------------------------------------------------------------------------------------//
//                                        Lexer                                         //
//--------------------------------------------------------------------------------------//

pub type LexResult = Result<TokenInfo, LexerError>;

/// Helper function to check if a token matches a predicate.
fn check_token(token: &LexResult, predicate: &impl Fn(&TokenKind) -> bool) -> bool {
    match token {
        Ok(TokenInfo { token, .. }) => predicate(token),
        _ => false,
    }
}

/// The lexer tokenize a program input source and **output a stream of tokens**. \
/// The lexer is a state machine that is used by the parser to generate an AST.
#[derive(Clone)]
pub struct Lexer<R>
where
    R: Read,
{
    reader: R,
    /// Everything successfully read from the source code.
    content: Vec<u8>,
    index: usize,
    line_info: LineInfo,
    pub operators: HashSet<String>,
    peeked_tokens: Vec<LexResult>, // Queue of peeked tokens (FIFO)
    /// The buffer size of the lexer. \
    /// This is the size of the buffer used to read from the source code.
    buffer_size: usize,
    /// If true, the lexer will try to read from the source code after reaching the end of the file.
    /// This is useful for streams where even if EOF is reached, **more data might be available later**.
    try_read_after_eof: bool,
    eof: bool,
    /// If true, the lexer will only read from the source code once.
    read_only_once: bool,
    has_read_once: bool,
    /// A flag to indicate that the next open paren is a function call paren
    /// This is used to differentiate between function calls and tuples
    pub is_function_call: bool,
}

impl<R: Read> Lexer<R> {
    /// Create a new lexer from a reader.
    /// The lexer will read from the reader until it reaches the end of the file.
    pub fn new(reader: R) -> Self {
        Self {
            reader,
            content: Vec::new(),
            index: 0,
            line_info: LineInfo::default(),
            operators: HashSet::new(),
            peeked_tokens: Vec::new(),
            buffer_size: 512,
            try_read_after_eof: false,
            eof: false,
            read_only_once: false,
            has_read_once: false,
            is_function_call: false,
        }
    }

    /// Create a new lexer from a reader.
    /// The lexer will read from the reader until it reaches the end of the stream, then the parser must call `refill_on_read` to refill the buffer.
    /// If the reader is a stream, the lexer will be able to refill the buffer with new data from the reader on the next read (at any time)
    pub fn new_stream(reader: R) -> Self {
        Self {
            reader,
            content: Vec::new(),
            index: 0,
            line_info: LineInfo::default(),
            operators: HashSet::new(),
            peeked_tokens: Vec::new(),
            buffer_size: 512,
            try_read_after_eof: true,
            eof: false,
            read_only_once: false,
            has_read_once: false,
            is_function_call: false,
        }
    }

    pub fn set_buffer_size(&mut self, buffer_size: usize) {
        self.buffer_size = buffer_size;
    }

    pub fn set_try_read_after_eof(&mut self, try_read_after_eof: bool) {
        self.try_read_after_eof = try_read_after_eof;
    }

    pub fn set_read_only_once(&mut self, read_only_once: bool) {
        self.read_only_once = read_only_once;
    }

    pub fn get_reader(&mut self) -> &mut R {
        &mut self.reader
    }

    pub fn current_index(&self) -> usize {
        self.index
    }

    pub fn set_index(&mut self, index: usize) {
        self.index = index;
    }

    /// Reset the lexer to its initial state with a new reader.
    /// In contrast to `reset`, this function will **also reset the reader**.
    pub fn reset_with_reader(&mut self, reader: R) {
        self.reader = reader;
        self.reset();
    }

    /// Reset the lexer to its initial state.
    /// This will clear the content, index, line info, peeked tokens, and EOF flag.
    /// This will **not reset the reader**.
    pub fn reset(&mut self) {
        self.content.clear();
        self.index = 0;
        self.line_info = LineInfo::default();
        self.peeked_tokens.clear();
        self.eof = false;
        self.has_read_once = false;
    }

    fn new_token_info(&self, token: TokenKind) -> LexResult {
        Ok(TokenInfo {
            info: self.line_info.clone(),
            token,
        })
    }

    /// Set the start of the token to the current position of the lexer.
    /// This function is called when the lexer is about to start reading a new token.
    fn set_info_start_to_current(&mut self) {
        self.line_info.start = self.line_info.end.clone();
    }

    /// This is the main function that reads from the source code.
    pub fn try_read_chunk(&mut self) -> Option<()> {
        // If EOF is reached and the lexer is not allowed to read after EOF, return None
        if self.eof && !self.try_read_after_eof {
            return None;
        }
        // Is the lexer allowed is only allowed to read once?
        if self.read_only_once && self.has_read_once {
            return None;
        }
        self.has_read_once = true;
        // Create a buffer to read from the source code
        let mut buffer = vec![0; self.buffer_size];
        match self.reader.read(&mut buffer) {
            Ok(n) => {
                if n == 0 {
                    self.eof = true;
                    None
                } else {
                    self.content.extend_from_slice(&buffer[..n]);
                    self.eof = false; // Reset EOF flag if new data is available
                    Some(()) // Success
                }
            }
            Err(e) => {
                match e.kind() {
                    ErrorKind::WouldBlock | ErrorKind::Interrupted => Some(()), // Success
                    _ => {
                        eprintln!("Error reading from the source: {}", e);
                        self.eof = true;
                        None
                    }
                }
            }
        }
    }

    fn peek_char(&mut self, offset: usize) -> Option<char> {
        let peek_index = self.index.checked_add(offset)?;
        if peek_index >= self.content.len() {
            self.try_read_chunk()?; // If fail, return None
        }
        let c = *self.content.get(peek_index)?;
        if c == 0 {
            None
        } else {
            Some(c as char)
        }
    }

    /// Read a string from the source code using a buffered reader.
    fn next_char(&mut self) -> Option<char> {
        if self.index >= self.content.len() {
            self.try_read_chunk()?; // If fail, return None
        }
        let c = *self.content.get(self.index)?;
        if c == 0 {
            return None;
        }
        self.index += 1;
        self.line_info.end.index += 1;
        self.line_info.end.column += 1;
        Some(c as char)
    }

    fn get_peeked_token(&mut self, offset: usize) -> Option<LexResult> {
        if self.peeked_tokens.is_empty() || offset >= self.peeked_tokens.len() {
            None
        } else {
            Some(self.peeked_tokens[offset].clone())
        }
    }

    fn consume_peeked_token(&mut self, offset: usize) -> Option<LexResult> {
        if self.peeked_tokens.is_empty() || offset >= self.peeked_tokens.len() {
            None
        } else {
            Some(self.peeked_tokens.remove(offset))
        }
    }

    /// Peek the next token from the source code.
    /// ! Note: This function should not be called from read_next_token() because it will cause an infinite loop (stack overflow)
    pub fn peek_token(&mut self, offset: usize) -> LexResult {
        if let Some(t) = self.get_peeked_token(offset) {
            t
        } else {
            let token = self.consume_next_token();
            // Do not push EOF to the peeked tokens list
            if let Ok(TokenInfo {
                token: TokenKind::EndOfFile,
                ..
            }) = token
            {
                return token;
            }
            self.peeked_tokens.push(token.to_owned());
            self.peek_token(offset)
        }
    }

    /// Get the next token from the source code,
    /// Or return the next peeked token.
    pub fn next_token(&mut self) -> LexResult {
        if let Some(token) = self.consume_peeked_token(0) {
            token
        } else {
            self.consume_next_token()
        }
    }

    /// Expect there to be a token (not EOF).
    /// If there is a token, return it.
    /// Else, try to read from the source if the reader is a stream.
    /// Then return the first read token (can be EOF but will throw error later)
    fn expect_next_token(&mut self) -> LexResult {
        let token = self.next_token()?;
        if token.token == TokenKind::EndOfFile {
            self.next_token()
        } else {
            Ok(token)
        }
    }

    /// Peek the next token from the source code, ignoring tokens that match the predicate.
    /// Or return the next peeked token.
    pub fn peek_token_not(&mut self, predicate: impl Fn(&TokenKind) -> bool) -> LexResult {
        let mut idx = 0usize;
        let mut token = self.peek_token(idx);
        while check_token(&token, &predicate) {
            token = self.peek_token(idx + 1);
            idx += 1;
        }
        token
    }

    /// Get the next token from the source code, ignoring tokens that match the predicate.
    /// Or return the next peeked token.
    pub fn read_next_token_not(&mut self, predicate: impl Fn(&TokenKind) -> bool) -> LexResult {
        let mut token = self.next_token();
        while check_token(&token, &predicate) {
            token = self.next_token();
        }
        token
    }

    /// Expect there to be a token (not EOF) while ignoring tokens that match the predicate.
    /// If there is a token, return it.
    /// Else, try to read from the source if the reader is a stream.
    /// Then return the first read token (can be EOF but will throw error later)
    /// Or return the next peeked token (again, ignoring newlines)
    /// Direct copy of `read_next_token_no_nl` but expecting tokens from the stream.
    pub fn expect_next_token_not(&mut self, predicate: impl Fn(&TokenKind) -> bool) -> LexResult {
        let mut token = self.expect_next_token();
        while check_token(&token, &predicate) {
            token = self.expect_next_token();
        }
        token
    }

    /// Get the next token from the source code.
    /// This function is the main function of the lexer.
    /// This function can only move forward, it cannot go back.
    fn consume_next_token(&mut self) -> LexResult {
        self.set_info_start_to_current();
        if let Some(c) = self.next_char() {
            if c == ' ' || c == '\t' || c == '\r' {
                self.consume_next_token() // Ignore whitespace
            } else if c == '\n' {
                self.line_info.end.line += 1;
                self.line_info.end.column = 1;
                self.new_token_info(TokenKind::Newline)
            } else if c == '"' {
                self.read_string()
            } else if c == '\'' {
                self.read_char()
            } else if c.is_numeric() {
                self.read_number(c)
            } else if Self::is_identifier_head_char(c) {
                let id = self.read_identifier(c)?;
                if id.token.is_identifier() {
                    // Check if the next character is a open paren
                    if self.peek_char(0) == Some('(') {
                        self.is_function_call = true;
                    }
                }
                Ok(id)
            } else {
                let token = self.new_token_info(match c {
                    '(' => TokenKind::LeftParen {
                        is_function_call: self.is_function_call,
                    },
                    ')' => TokenKind::RightParen,
                    '{' => TokenKind::LeftBrace,
                    '}' => TokenKind::RightBrace,
                    '[' => TokenKind::LeftBracket,
                    ']' => TokenKind::RightBracket,
                    ';' => TokenKind::SemiColon,
                    ':' => TokenKind::Colon,
                    ',' => TokenKind::Comma,
                    '/' if self.peek_char(0) == Some('/') => return self.read_comment(),
                    _ => return self.read_operator(c),
                });
                self.is_function_call = false; // Reset the function call flag
                token
            }
        } else {
            self.new_token_info(TokenKind::EndOfFile)
        }
    }

    pub fn resolve_escape_sequence(s: String) -> String {
        let s = regex_replace_all!(r#"\\u(\d{4})"#, &s, |_, num: &str| {
            let num: u32 = u32::from_str_radix(num, 16).unwrap();
            let c: char = std::char::from_u32(num).unwrap();
            c.to_string()
        });
        let s = regex_replace_all!(r#"\\x(\d{2})"#, &s, |_, num: &str| {
            let num: u32 = u32::from_str_radix(num, 16).unwrap();
            let c: char = std::char::from_u32(num).unwrap();
            c.to_string()
        });
        let s = regex_replace_all!(r#"\\n"#, &s, |_| "\n".to_string());
        let s = regex_replace_all!(r#"\\r"#, &s, |_| "\r".to_string());
        let s = regex_replace_all!(r#"\\t"#, &s, |_| "\t".to_string());
        let s = regex_replace_all!(r#"\\0"#, &s, |_| "\0".to_string());
        // let s = regex_replace_all!(r#"\\\""#, &s, |_| "\"".to_string());
        let s = regex_replace_all!(r#"\\'"#, &s, |_| "'".to_string());
        let s = regex_replace_all!(r#"\\\\"#, &s, |_| "\\".to_string());
        s.to_string()
    }

    /// Helper function to read a string from the source code using a predicate to continue reading.
    ///
    /// # Arguments
    /// * `init` - The initial value of the token.
    /// * `cond` - The condition to continue reading the token, taking the current lexer and the currently peeked character.
    ///   If the condition is false, the token is returned.
    /// * `allow_eof_before_cond_false` - If true, the function will return the token even if the end of the file is reached before the condition is false.
    fn read_while(
        &mut self,
        init: Option<String>,
        cond: impl Fn(&mut Self, char) -> Result<bool, LexerError>,
        allow_eof_before_cond_false: bool,
    ) -> Result<String, LexerError> {
        let mut result = init.unwrap_or_default();
        while let Some(c) = self.peek_char(0) {
            if cond(self, c)? {
                self.next_char();
                result.push(c);
            } else {
                return Ok(result);
            }
        }
        if allow_eof_before_cond_false {
            Ok(result)
        } else {
            Err(LexerError::unexpected_end_of_file(self.line_info.clone()))
        }
    }

    fn read_quoted(
        &mut self,
        quote: char,
        mut build_token: impl FnMut(&mut Self, String) -> LexResult,
    ) -> LexResult {
        let escape = Cell::new(false);
        let s = self.read_while(
            None,
            move |_, c| {
                if escape.get() {
                    escape.set(false);
                    Ok(true)
                } else {
                    if c == '\\' {
                        escape.set(true);
                    }
                    Ok(c != quote)
                }
            },
            false,
        )?;
        let t = build_token(self, Lexer::<R>::resolve_escape_sequence(s));
        self.next_char(); // Eat the last quote
        t
    }

    /// Read a string from the source code.
    fn read_string(&mut self) -> LexResult {
        self.read_quoted('"', |this, s| this.new_token_info(TokenKind::String(s)))
    }

    /// Read a character from the source code.
    fn read_char(&mut self) -> LexResult {
        self.read_quoted('\'', |this, s| {
            if s.len() != 1 {
                Err(LexerError::invalid_char(s, this.line_info.clone()))
            } else {
                this.new_token_info(TokenKind::Char(s.chars().next().unwrap()))
            }
        })
    }

    /// Parse the float number suffix: `0.3f32`, `0.3f64` or `0.3fbig`
    fn read_number_suffix_float(
        &mut self,
        ty: &RefCell<Option<NumInfo>>,
    ) -> Result<bool, LexerError> {
        match (self.peek_char(1), self.peek_char(2)) {
            // f32
            (Some('3'), Some('2')) => {
                self.next_char(); // f
                self.next_char(); // 3
                self.next_char(); // 2
                self.set_number_ty(ty, NumInfo::float(BitSize::Bit32))?;
            }
            // f64
            (Some('6'), Some('4')) => {
                self.next_char(); // f
                self.next_char(); // 6
                self.next_char(); // 4
                self.set_number_ty(ty, NumInfo::float(BitSize::Bit64))?;
            }
            // fbig
            (Some('b'), Some('i')) => {
                if self.peek_char(3) == Some('g') {
                    self.next_char(); // f
                    self.next_char(); // b
                    self.next_char(); // i
                    self.next_char(); // g
                    self.set_number_ty(ty, NumInfo::float(BitSize::BitVar))?;
                }
            }
            _ => (),
        }
        Ok(false)
    }

    /// Parse the integer number suffix: `42i8`, `42i16`, `42i32`, `42i64`, `42i128` or `42ibig`
    fn read_number_suffix_int(
        &mut self,
        ty: &RefCell<Option<NumInfo>>,
    ) -> Result<bool, LexerError> {
        match self.peek_char(1) {
            // i8
            Some('8') => {
                self.next_char(); // i
                self.next_char(); // 8
                self.set_number_ty(ty, NumInfo::int(BitSize::Bit8))?;
            }
            // i16 or i128
            Some('1') => match self.peek_char(2) {
                Some('6') => {
                    self.next_char(); // i
                    self.next_char(); // 1
                    self.next_char(); // 6
                    self.set_number_ty(ty, NumInfo::int(BitSize::Bit16))?;
                }
                Some('2') => {
                    if self.peek_char(3) == Some('8') {
                        self.next_char(); // i
                        self.next_char(); // 1
                        self.next_char(); // 2
                        self.next_char(); // 8
                        self.set_number_ty(ty, NumInfo::int(BitSize::Bit128))?;
                    }
                }
                _ => (),
            },
            // i32
            Some('3') => {
                if self.peek_char(2) == Some('2') {
                    self.next_char(); // i
                    self.next_char(); // 3
                    self.next_char(); // 2
                    self.set_number_ty(ty, NumInfo::int(BitSize::Bit32))?;
                }
            }
            // i64
            Some('6') => {
                if self.peek_char(2) == Some('4') {
                    self.next_char(); // i
                    self.next_char(); // 6
                    self.next_char(); // 4
                    self.set_number_ty(ty, NumInfo::int(BitSize::Bit64))?;
                }
            }
            // ibig
            Some('b') => {
                if self.peek_char(2) == Some('i') && self.peek_char(3) == Some('g') {
                    self.next_char(); // i
                    self.next_char(); // b
                    self.next_char(); // i
                    self.next_char(); // g
                    self.set_number_ty(ty, NumInfo::int(BitSize::BitVar))?;
                }
            }
            _ => (),
        }
        Ok(false)
    }

    /// Parse the unsigned integer number suffix `1u1`, `42u8`, `42u16`, `42u32`, `42u64`, `42u128` or `42ubig`
    fn read_number_suffix_uint(
        &mut self,
        ty: &RefCell<Option<NumInfo>>,
    ) -> Result<bool, LexerError> {
        match self.peek_char(1) {
            // u1, u16 or u128
            Some('1') => match self.peek_char(2) {
                Some('6') => {
                    self.next_char(); // u
                    self.next_char(); // 1
                    self.next_char(); // 6
                    self.set_number_ty(ty, NumInfo::uint(BitSize::Bit16))?;
                }
                Some('2') => {
                    if self.peek_char(2) == Some('8') {
                        self.next_char(); // u
                        self.next_char(); // 1
                        self.next_char(); // 2
                        self.next_char(); // 8
                        self.set_number_ty(ty, NumInfo::uint(BitSize::Bit128))?;
                    }
                }
                _ => {
                    self.next_char(); // u
                    self.next_char(); // 1
                    self.set_number_ty(ty, NumInfo::uint(BitSize::Bit1))?;
                }
            },

            // u8
            Some('8') => {
                self.next_char(); // u
                self.next_char(); // 8
                self.set_number_ty(ty, NumInfo::uint(BitSize::Bit8))?;
            }
            // u32
            Some('3') => {
                if self.peek_char(2) == Some('2') {
                    self.next_char(); // u
                    self.next_char(); // 3
                    self.next_char(); // 2
                    self.set_number_ty(ty, NumInfo::uint(BitSize::Bit32))?;
                }
            }
            // u64
            Some('6') => {
                if self.peek_char(2) == Some('4') {
                    self.next_char(); // u
                    self.next_char(); // 6
                    self.next_char(); // 4
                    self.set_number_ty(ty, NumInfo::uint(BitSize::Bit64))?;
                }
            }
            // ubig
            Some('b') => {
                if self.peek_char(2) == Some('i') && self.peek_char(3) == Some('g') {
                    self.next_char(); // u
                    self.next_char(); // b
                    self.next_char(); // i
                    self.next_char(); // g
                    self.set_number_ty(ty, NumInfo::uint(BitSize::BitVar))?;
                }
            }
            _ => (),
        }
        Ok(false)
    }

    fn parse_number_type(&self, s: &str, num: NumInfo) -> Result<Number, LexerError> {
        match num {
            NumInfo {
                float: false,
                signed: false,
                bits: BitSize::Bit1,
            } => {
                if let Ok(i) = s.parse::<u8>() {
                    if i > 1 {
                        Err(LexerError::invalid_number_type(
                            s,
                            &std_types::UINT1,
                            self.line_info.clone(),
                        ))
                    } else {
                        Ok(Number::UnsignedInteger(UnsignedInteger::UInt1(i)))
                    }
                } else {
                    Err(LexerError::invalid_number_type(
                        s,
                        &std_types::UINT1,
                        self.line_info.clone(),
                    ))
                }
            }
            NumInfo {
                float: false,
                signed: false,
                bits: BitSize::Bit8,
            } => {
                if let Ok(i) = s.parse::<u8>() {
                    Ok(Number::UnsignedInteger(UnsignedInteger::UInt8(i)))
                } else {
                    Err(LexerError::invalid_number_type(
                        s,
                        &std_types::UINT8,
                        self.line_info.clone(),
                    ))
                }
            }
            NumInfo {
                float: false,
                signed: false,
                bits: BitSize::Bit16,
            } => {
                if let Ok(i) = s.parse::<u16>() {
                    Ok(Number::UnsignedInteger(UnsignedInteger::UInt16(i)))
                } else {
                    Err(LexerError::invalid_number_type(
                        s,
                        &std_types::UINT16,
                        self.line_info.clone(),
                    ))
                }
            }
            NumInfo {
                float: false,
                signed: false,
                bits: BitSize::Bit32,
            } => {
                if let Ok(i) = s.parse::<u32>() {
                    Ok(Number::UnsignedInteger(UnsignedInteger::UInt32(i)))
                } else {
                    Err(LexerError::invalid_number_type(
                        s,
                        &std_types::UINT32,
                        self.line_info.clone(),
                    ))
                }
            }
            NumInfo {
                float: false,
                signed: false,
                bits: BitSize::Bit64,
            } => {
                if let Ok(i) = s.parse::<u64>() {
                    Ok(Number::UnsignedInteger(UnsignedInteger::UInt64(i)))
                } else {
                    Err(LexerError::invalid_number_type(
                        s,
                        &std_types::UINT64,
                        self.line_info.clone(),
                    ))
                }
            }
            NumInfo {
                float: false,
                signed: false,
                bits: BitSize::Bit128,
            } => {
                if let Ok(i) = s.parse::<u128>() {
                    Ok(Number::UnsignedInteger(UnsignedInteger::UInt128(i)))
                } else {
                    Err(LexerError::invalid_number_type(
                        s,
                        &std_types::UINT128,
                        self.line_info.clone(),
                    ))
                }
            }
            NumInfo {
                float: false,
                signed: false,
                bits: BitSize::BitVar,
            } => {
                if let Ok(i) = Natural::from_str(s) {
                    Ok(Number::UnsignedInteger(UnsignedInteger::UIntVar(i)))
                } else {
                    Err(LexerError::invalid_number_type(
                        s,
                        &std_types::UINTBIG,
                        self.line_info.clone(),
                    ))
                }
            }
            NumInfo {
                float: false,
                signed: true,
                bits: BitSize::Bit8,
            } => {
                if let Ok(i) = s.parse::<i8>() {
                    Ok(Number::SignedInteger(SignedInteger::Int8(i)))
                } else {
                    Err(LexerError::invalid_number_type(
                        s,
                        &std_types::INT8,
                        self.line_info.clone(),
                    ))
                }
            }
            NumInfo {
                float: false,
                signed: true,
                bits: BitSize::Bit16,
            } => {
                if let Ok(i) = s.parse::<i16>() {
                    Ok(Number::SignedInteger(SignedInteger::Int16(i)))
                } else {
                    Err(LexerError::invalid_number_type(
                        s,
                        &std_types::INT16,
                        self.line_info.clone(),
                    ))
                }
            }
            NumInfo {
                float: false,
                signed: true,
                bits: BitSize::Bit32,
            } => {
                if let Ok(i) = s.parse::<i32>() {
                    Ok(Number::SignedInteger(SignedInteger::Int32(i)))
                } else {
                    Err(LexerError::invalid_number_type(
                        s,
                        &std_types::INT32,
                        self.line_info.clone(),
                    ))
                }
            }
            NumInfo {
                float: false,
                signed: true,
                bits: BitSize::Bit64,
            } => {
                if let Ok(i) = s.parse::<i64>() {
                    Ok(Number::SignedInteger(SignedInteger::Int64(i)))
                } else {
                    Err(LexerError::invalid_number_type(
                        s,
                        &std_types::INT64,
                        self.line_info.clone(),
                    ))
                }
            }
            NumInfo {
                float: false,
                signed: true,
                bits: BitSize::Bit128,
            } => {
                if let Ok(i) = s.parse::<i128>() {
                    Ok(Number::SignedInteger(SignedInteger::Int128(i)))
                } else {
                    Err(LexerError::invalid_number_type(
                        s,
                        &std_types::INT128,
                        self.line_info.clone(),
                    ))
                }
            }
            NumInfo {
                float: false,
                signed: true,
                bits: BitSize::BitVar,
            } => {
                if let Ok(i) = Integer::from_str(s) {
                    Ok(Number::SignedInteger(SignedInteger::IntVar(i)))
                } else {
                    Err(LexerError::invalid_number_type(
                        s,
                        &std_types::INTBIG,
                        self.line_info.clone(),
                    ))
                }
            }
            NumInfo {
                float: true,
                bits: BitSize::Bit32,
                ..
            } => {
                if let Ok(f) = s.parse::<f32>() {
                    Ok(Number::FloatingPoint(FloatingPoint::Float32(f)))
                } else {
                    Err(LexerError::invalid_number_type(
                        s,
                        &std_types::FLOAT32,
                        self.line_info.clone(),
                    ))
                }
            }
            NumInfo {
                float: true,
                bits: BitSize::Bit64,
                ..
            } => {
                if let Ok(f) = s.parse::<f64>() {
                    Ok(Number::FloatingPoint(FloatingPoint::Float64(f)))
                } else {
                    Err(LexerError::invalid_number_type(
                        s,
                        &std_types::FLOAT64,
                        self.line_info.clone(),
                    ))
                }
            }
            NumInfo {
                float: true,
                bits: BitSize::BitVar,
                ..
            } => {
                if let Some(f) = Rational::from_sci_string(s) {
                    Ok(Number::FloatingPoint(FloatingPoint::FloatBig(f)))
                } else {
                    Err(LexerError::invalid_number_type(
                        s,
                        &std_types::FLOATBIG,
                        self.line_info.clone(),
                    ))
                }
            }
            _ => Err(LexerError::invalid_number_type(
                s,
                &std_types::NUM(),
                self.line_info.clone(),
            )),
        }
    }

    /// Default to `f32` if no suffix is provided
    fn parse_number_float(&self, s: &str) -> Result<Number, LexerError> {
        if let Ok(f) = s.parse::<f32>() {
            Ok(Number::FloatingPoint(FloatingPoint::Float32(f)))
        } else {
            Err(LexerError::invalid_number(s, self.line_info.clone()))
        }
    }

    // Fit the number into the smallest possible unsigned or signed integer
    fn parse_number_int(&self, s: &str) -> Result<Number, LexerError> {
        if let Some(s) = s.strip_prefix('-') {
            let i = s.parse::<i128>();
            if i.is_err() {
                if let Ok(i) = Integer::from_str(s) {
                    return Ok(Number::SignedInteger(SignedInteger::IntVar(i)));
                }
                return Err(LexerError::invalid_number(s, self.line_info.clone()));
            }
            let i = i.unwrap();
            Ok(if i >= i8::MIN as i128 && i <= i8::MAX as i128 {
                Number::SignedInteger(SignedInteger::Int8(i as i8))
            } else if i >= i16::MIN as i128 && i <= i16::MAX as i128 {
                Number::SignedInteger(SignedInteger::Int16(i as i16))
            } else if i >= i32::MIN as i128 && i <= i32::MAX as i128 {
                Number::SignedInteger(SignedInteger::Int32(i as i32))
            } else if i >= i64::MIN as i128 && i <= i64::MAX as i128 {
                Number::SignedInteger(SignedInteger::Int64(i as i64))
            } else {
                Number::SignedInteger(SignedInteger::Int128(i))
            })
        } else {
            let u = s.parse::<u128>();
            if u.is_err() {
                if let Ok(u) = Natural::from_str(s) {
                    return Ok(Number::UnsignedInteger(UnsignedInteger::UIntVar(u)));
                }
                return Err(LexerError::invalid_number(s, self.line_info.clone()));
            }
            let u = u.unwrap();
            Ok(if u <= 1 {
                Number::UnsignedInteger(UnsignedInteger::UInt1(u as u8))
            } else if u >= u8::MIN as u128 && u <= u8::MAX as u128 {
                Number::UnsignedInteger(UnsignedInteger::UInt8(u as u8))
            } else if u >= u16::MIN as u128 && u <= u16::MAX as u128 {
                Number::UnsignedInteger(UnsignedInteger::UInt16(u as u16))
            } else if u >= u32::MIN as u128 && u <= u32::MAX as u128 {
                Number::UnsignedInteger(UnsignedInteger::UInt32(u as u32))
            } else if u >= u64::MIN as u128 && u <= u64::MAX as u128 {
                Number::UnsignedInteger(UnsignedInteger::UInt64(u as u64))
            } else {
                Number::UnsignedInteger(UnsignedInteger::UInt128(u))
            })
        }
    }

    fn set_number_ty(&self, rc: &RefCell<Option<NumInfo>>, ty: NumInfo) -> Result<(), LexerError> {
        if let Some(ty) = rc.replace(Some(ty)) {
            Err(LexerError::new(
                format!("Type already set to {:?}", ty),
                self.line_info.clone(),
            ))
        } else {
            Ok(())
        }
    }

    /// Read a number from the source code.
    /// Can be an integer or a float (casted at runtime).
    fn read_number(&mut self, c: char) -> LexResult {
        let has_dot = Cell::new(false);
        let ty: RefCell<Option<NumInfo>> = RefCell::new(None);
        // Set the type of the number and crash if a type is already set
        let s = self.read_while(
            Some(c.to_string()),
            |this, c| match c {
                '.' => {
                    // Parse the float number suffix 0.3
                    Ok(if has_dot.get() {
                        false
                    } else {
                        has_dot.set(true);
                        true
                    })
                }
                'f' => this.read_number_suffix_float(&ty), // Always return false
                'i' => this.read_number_suffix_int(&ty),   // Always return false
                'u' => this.read_number_suffix_uint(&ty),  // Always return false
                'e' => {
                    // Allow scientific notation: `1.0e-2`
                    this.set_number_ty(&ty, NumInfo::float(BitSize::BitVar))?;
                    Ok(true) // Always return true to allow the exponent after the 'e'
                }
                '+' | '-' => {
                    // If still parsing after `ty = FLOATBIG`, then it's an exponent
                    Ok(if let Some(ty) = ty.borrow().as_ref() {
                        *ty == NumInfo::float(BitSize::BitVar)
                    } else {
                        false // Otherwise, it's something else
                    })
                }
                _ => Ok(c.is_numeric()),
            },
            true,
        )?;
        self.new_token_info(TokenKind::Number(if let Some(ty) = ty.take() {
            self.parse_number_type(&s, ty)?
        } else if has_dot.get() {
            self.parse_number_float(&s)?
        } else {
            self.parse_number_int(&s)?
        }))
    }

    fn is_identifier_head_char(c: char) -> bool {
        c.is_alphabetic() || c == '_'
    }
    fn is_identifier_body_char(c: char) -> bool {
        c.is_alphanumeric() || c == '_'
    }

    fn create_identifier_type_or_keyword(&self, s: String) -> TokenKind {
        match s.as_str() {
            "true" => TokenKind::Boolean(true),
            "false" => TokenKind::Boolean(false),
            "let" => TokenKind::Let,
            sym => match self.operators.get(sym) {
                Some(op) => TokenKind::Op(op.clone()),
                None => TokenKind::Identifier(s),
            },
        }
    }

    /// Read an identifier from the source code.
    fn read_identifier(&mut self, c: char) -> LexResult {
        let s = self.read_while(
            Some(c.to_string()),
            |_, c| Ok(Self::is_identifier_body_char(c)),
            true,
        )?;
        self.new_token_info(self.create_identifier_type_or_keyword(s))
    }

    /// Read a comment from the source code.
    fn read_comment(&mut self) -> LexResult {
        self.next_char(); // Eat the first '/'
        let s = self.read_while(None, |_, c| Ok(c != '\n'), true)?;
        self.new_token_info(TokenKind::Comment(s))
    }

    fn read_operator(&mut self, first: char) -> LexResult {
        // get all self.operators starting with first
        let mut ops = self
            .operators
            .iter()
            .filter(|op| op.starts_with(first))
            .cloned()
            .collect::<HashSet<String>>();
        if ops.is_empty() {
            return Err(LexerError::unexpected_character(
                first,
                self.line_info.clone(),
            ));
        }
        let mut longest_match = first.to_string();
        while let Some(c) = self.peek_char(0) {
            longest_match.push(c);
            let new_ops = ops
                .into_iter()
                .filter(|op| op.starts_with(&longest_match))
                .collect::<HashSet<_>>();
            if new_ops.is_empty() {
                longest_match.pop();
                break;
            }
            ops = new_ops;
            self.next_char(); // Eat the peeked character
        }
        let op = self.operators.get(&longest_match).unwrap(); // Safe because we know the operator exists
        self.new_token_info(TokenKind::Op(op.clone()))
    }
}

pub fn from_str(input: &str) -> Lexer<BytesReader<'_>> {
    Lexer::new(BytesReader::from(input))
}

pub fn from_string(input: String) -> Lexer<Cursor<String>> {
    Lexer::new(Cursor::new(input))
}

pub fn from_file(file: File) -> Lexer<BufReader<File>> {
    Lexer::new(BufReader::new(file))
}

pub fn from_path(path: PathBuf) -> Result<Lexer<BufReader<File>>, Error> {
    Ok(Lexer::new(BufReader::new(File::open(path.clone())?)))
}

pub fn from_stream<R: Read>(reader: R) -> Lexer<R> {
    Lexer::new_stream(reader)
}

pub fn from_stdin() -> Lexer<StdinReader> {
    from_stream(StdinReader::default())
}
