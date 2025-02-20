/// Runtime error
#[derive(Debug, Clone)]
pub struct RuntimeError {
    pub message: String,
}

pub fn runtime_error(message: String) -> RuntimeError {
    RuntimeError { message }
}
