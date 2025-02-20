#[cfg(test)]
mod tests {
    use std::sync::Arc;

    use cranelift_codegen::isa::{lookup, Builder, LookupError};
    use cranelift_codegen::settings::{self, Configurable};
    use target_lexicon::triple;

    use super::super::Cranelift;
    use crate::parser::parser::parse_str_all;
    use crate::{
        compiler::compiler::{Backend, CompileOptions, OptimizationLevel},
        lexer::lexer::InputSource,
    };

    fn build_isa(
        target: target_lexicon::Triple,
    ) -> Arc<dyn cranelift_codegen::isa::TargetIsa + 'static> {
        let isa_builder: Result<Builder, LookupError> = lookup(target.clone());
        if let Err(e) = isa_builder {
            panic!("Cannot compile for target: {}. Error: {}", target, e);
        }
        let mut isa_builder = isa_builder.unwrap();
        if target.architecture == target_lexicon::Architecture::X86_64 {
            // See: https://github.com/rust-lang/rustc_codegen_cranelift/blob/07633821ed63360d4d7464998c29f4f588930a03/src/lib.rs#L335
            isa_builder.enable("nehalem").unwrap();
        }
        let isa = isa_builder.finish(settings::Flags::new(settings::builder()));
        if let Err(e) = isa {
            panic!("Failed to build ISA: {}", e);
        }
        isa.unwrap()
    }

    fn default_options(target: target_lexicon::Triple) -> CompileOptions<std::io::Sink> {
        CompileOptions::new(
            target,
            std::io::sink(),
            InputSource::String,
            OptimizationLevel::None,
            false,
            false,
        )
    }

    /// Test that the Cranelift backend can compile a simple "Hello, World!" program on Windows.
    /// The test uses the x86_64-unknown-windows-msvc target.
    ///
    /// ## References
    /// - [Cranelift backend for Rust: `/src/lib.rs`](https://github.com/rust-lang/rustc_codegen_cranelift/blob/07633821ed63360d4d7464998c29f4f588930a03/src/lib.rs#L344)
    #[test]
    fn test_cranelift_print_hello_world_windows() {
        let target = triple!("x86_64-unknown-windows-msvc");
        let isa = build_isa(target.clone());
        let mut cranelift = Cranelift::new(isa, settings::Flags::new(settings::builder()));
        let module = parse_str_all(r#"print("Hello, World!");"#, None).expect("Failed to parse");
        let result = cranelift.compile_module(&module, default_options(target));
        assert!(result.is_ok());
    }

    /// Test that the Cranelift backend can compile a simple "Hello, World!" program on Linux.
    /// The test uses the x86_64-unknown-linux-gnu target.
    #[test]
    fn test_cranelift_print_hello_world_linux() {
        let target = triple!("x86_64-unknown-linux-gnu");
        let isa = build_isa(target.clone());
        let mut cranelift = Cranelift::new(isa, settings::Flags::new(settings::builder()));
        let module = parse_str_all(r#"print("Hello, World!");"#, None).expect("Failed to parse");
        let result = cranelift.compile_module(&module, default_options(target));
        assert!(result.is_ok());
    }
}
