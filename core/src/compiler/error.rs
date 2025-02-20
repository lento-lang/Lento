/// Lexer error
#[derive(Debug, Clone)]
pub struct CompileError {
    pub message: String,
}

pub fn compile_error(message: String) -> CompileError {
    CompileError { message }
}
