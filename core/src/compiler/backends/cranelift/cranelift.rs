use std::{io::Write, sync::Arc};

use cranelift_codegen::{isa, settings};
use cranelift_object::{ObjectBuilder, ObjectModule};

use crate::{
    compiler::compiler::{Backend, CompileOptions, CompileResult},
    type_checker::checked_ast::CheckedAst,
};

pub struct Cranelift {
    isa: Arc<dyn isa::TargetIsa>,
    _flags: settings::Flags,
}

impl Cranelift {
    pub fn new(isa: Arc<dyn isa::TargetIsa>, flags: settings::Flags) -> Self {
        Self { isa, _flags: flags }
    }
}

impl Cranelift {
    fn make_module<Out: Write>(
        &mut self,
        filename: Option<&str>,
        options: &CompileOptions<Out>,
    ) -> ObjectModule {
        let mut builder = ObjectBuilder::new(
            self.isa.clone(),
            filename.unwrap_or("module"),
            cranelift_module::default_libcall_names(),
        )
        .unwrap();
        builder.per_function_section(options.function_sections);
        ObjectModule::new(builder)
    }
}

impl<Out: Write> Backend<Out> for Cranelift {
    fn compile(
        &mut self,
        _exprs: &[CheckedAst],
        filename: Option<&str>,
        options: CompileOptions<Out>,
    ) -> CompileResult {
        let mut _obj_module = self.make_module(filename, &options);
        todo!()
    }
}
