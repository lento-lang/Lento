/*
#[cfg(test)]
mod tests {
    use std::io::Write;
    use std::sync::Arc;

    use cranelift_codegen::isa::{lookup, Builder, LookupError};
    use cranelift_codegen::settings::{self, Configurable};
    use target_lexicon::triple;

    use super::super::Cranelift;
    use crate::parser::parser::{self, ParseResults};
    use crate::stdlib::init::{stdlib, Initializer};

    use crate::type_checker::checker::TypeChecker;
    use crate::{
        compiler::compiler::{Backend, CompileOptions, OptimizationLevel},
        lexer::lexer::InputSource,
    };

    fn parse_str_all(input: &str, init: Option<&Initializer>) -> ParseResults {
        let mut parser = parser::from_str(input);
        if let Some(init) = init {
            init.init_parser(&mut parser);
        }
        parser.parse_all()
    }

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

    fn default_options<Out: Write>(
        output_file: Out,
        target: target_lexicon::Triple,
    ) -> CompileOptions<Out> {
        CompileOptions::new(
            target,
            output_file,
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
        let exprs = parse_str_all(r#"print("Hello, World!");"#, None).expect("Failed to parse");
        let mut checker = TypeChecker::default();
        stdlib().init_type_checker(&mut checker);
        let checked = checker.check_top_exprs(&exprs).expect("Failed to check");
        let mut program_output = Vec::new();
        let result = cranelift.compile(
            &checked,
            Some("example.o"),
            default_options(&mut program_output, target),
        );
        assert!(result.is_ok());
        assert!(!program_output.is_empty());
    }

    /// Test that the Cranelift backend can compile a simple "Hello, World!" program on Linux.
    /// The test uses the x86_64-unknown-linux-gnu target.
    #[test]
    fn test_cranelift_print_hello_world_linux() {
        let target = triple!("x86_64-unknown-linux-gnu");
        let isa = build_isa(target.clone());
        let mut cranelift = Cranelift::new(isa, settings::Flags::new(settings::builder()));
        let exprs = parse_str_all(r#"print("Hello, World!");"#, None).expect("Failed to parse");
        let mut checker = TypeChecker::default();
        stdlib().init_type_checker(&mut checker);
        let checked = checker.check_top_exprs(&exprs).expect("Failed to check");
        let mut program_output = Vec::new();
        let result = cranelift.compile(
            &checked,
            Some("example.o"),
            default_options(&mut program_output, target),
        );
        assert!(result.is_ok());
        assert!(!program_output.is_empty());
    }
}
*/
