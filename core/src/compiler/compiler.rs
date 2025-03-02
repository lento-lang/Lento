use std::io::{BufWriter, Write};

use crate::{lexer::lexer::InputSource, type_checker::checked_ast::CheckedAst};

use super::error::CompileError;

use target_lexicon::Triple;

//--------------------------------------------------------------------------------------//
//                                      Compiler                                        //
//--------------------------------------------------------------------------------------//

/// Optimization level for the compiler. \
/// The optimization level is used to specify the amount of optimization to perform when compiling.
pub enum OptimizationLevel {
    None,
    Low,
    Medium,
    High,
    Max,
}

/// Generic *(backend-agnostic)* options for the compiler. \
/// The options are used to configure the compiler's behavior
/// when compiling a module.
pub struct CompileOptions<Out: Write> {
    /// The optimization level.
    pub optimization_level: OptimizationLevel,
    /// Whether to include debug information in the compiled module.
    pub debug_info: bool,
    /// The target triple.
    pub target: Triple,
    /// The output stream to write the compiled module to.
    pub output_stream: BufWriter<Out>,
    /// Input source info of the module.
    pub input_source: InputSource,
    /// Whether to emit functions into their own sections.
    pub function_sections: bool,
}

impl<Out: Write> CompileOptions<Out> {
    pub fn new(
        target: Triple,
        output_file: Out,
        input_source: InputSource,
        optimization_level: OptimizationLevel,
        debug_info: bool,
        function_sections: bool,
    ) -> Self {
        Self {
            optimization_level,
            debug_info,
            target,
            output_stream: BufWriter::new(output_file),
            input_source,
            function_sections,
        }
    }
}

/// The result of compiling a module. \
/// If the module compiles successfully, the result is `Ok(())`. \
/// If the module fails to compile, the result is `Err(CompileError)`.
pub type CompileResult = Result<(), CompileError>;

/// The backend trait is implemented by all compiler backends. \
/// A backend is responsible for compiling a module into, for example:
/// - Machine code (native executable)
/// - Bytecode (virtual machine code)
/// - Intermediate representation (IR)
/// - Text (assembly code)
/// - Translation to another languages such as:
///     - C
///     - JavaScript
///     - etc.
pub trait Backend<Out: Write> {
    /// The output type for the backend. \
    /// The output type is used to specify the output stream for the compiled module.
    /// For example, the output type could be a file, a buffer, or a stream.
    ///
    /// Compiles a module using the specified backend and options. \
    /// The method returns a `CompileResult` indicating whether the module compiled successfully.
    fn compile(
        &mut self,
        exprs: &[CheckedAst],
        filename: Option<&str>,
        options: CompileOptions<Out>,
    ) -> CompileResult
    where
        Self: Sized;
}

/// Compiles a module using the specified backend and options. \
/// The function returns a `CompileResult` indicating whether the module compiled successfully.
pub fn compile<Out: Write, B: Backend<Out>>(
    backend: &mut B,
    exprs: &[CheckedAst],
    filename: Option<&str>,
    options: CompileOptions<Out>,
) -> CompileResult {
    backend.compile(exprs, filename, options)
}
