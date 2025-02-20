#[cfg(test)]
mod tests {
    use crate::{
        interpreter::{
            environment::{global_env, Environment},
            interpreter::{interpret_ast, interpret_module},
            number::{FloatingPoint, Number, UnsignedInteger},
            value::Value,
        },
        lexer::token::LineInfo,
        parser::parser,
        stdlib::init::stdlib,
        type_checker::{
            checked_ast::{CheckedAst, CheckedFunction, CheckedParam},
            checker::TypeChecker,
            types::{std_types, GetType, Type, TypeTrait},
        },
    };

    fn std_env() -> Environment<'static> {
        let mut env = global_env();
        stdlib().init_environment(&mut env);
        env
    }

    fn make_u8(n: u8) -> Value {
        Value::Number(Number::UnsignedInteger(UnsignedInteger::UInt8(n)))
    }

    fn make_f32(n: f32) -> Value {
        Value::Number(Number::FloatingPoint(FloatingPoint::Float32(n)))
    }

    fn add(lhs: CheckedAst, rhs: CheckedAst) -> CheckedAst {
        CheckedAst::Call {
            function: Box::new(CheckedAst::Call {
                function: Box::new(CheckedAst::Identifier(
                    "add".into(),
                    std_types::NUM(),
                    LineInfo::default(),
                )),
                arg: Box::new(rhs),
                return_type: std_types::NUM(),
                info: LineInfo::default(),
            }),
            arg: Box::new(lhs),
            return_type: std_types::NUM(),
            info: LineInfo::default(),
        }
    }

    fn fn_unit() -> CheckedAst {
        CheckedAst::Function(
            Box::new(CheckedFunction::new(
                CheckedParam {
                    name: "ignore".to_string(),
                    ty: std_types::UNIT,
                },
                CheckedAst::Block(vec![], std_types::UNIT, LineInfo::default()),
                std_types::UNIT,
            )),
            LineInfo::default(),
        )
    }

    #[test]
    fn binary_add() {
        let ast = add(
            CheckedAst::Literal(make_u8(1), LineInfo::default()),
            CheckedAst::Literal(make_u8(2), LineInfo::default()),
        );
        let result = interpret_ast(&ast, &mut std_env());
        assert!(result.is_ok());
        let result = result.unwrap();
        assert!(result.get_type().equals(&std_types::UINT8));
        assert_eq!(result, make_u8(3));
    }

    #[test]
    fn tuple() {
        let ast = CheckedAst::Tuple(
            vec![
                CheckedAst::Literal(make_u8(1), LineInfo::default()),
                CheckedAst::Literal(make_u8(2), LineInfo::default()),
                CheckedAst::Literal(make_u8(3), LineInfo::default()),
            ],
            Type::Tuple(vec![std_types::UINT8; 3]),
            LineInfo::default(),
        );
        let result = interpret_ast(&ast, &mut std_env());
        assert!(result.is_ok());
        let result = result.unwrap();
        assert!(result
            .get_type()
            .equals(&Type::Tuple(vec![std_types::UINT8; 3])));
        assert_eq!(
            result,
            Value::Tuple(
                vec![make_u8(1), make_u8(2), make_u8(3)],
                Type::Tuple(vec![std_types::UINT8; 3])
            )
        );
    }

    #[test]
    fn function_call() {
        let ast = add(
            CheckedAst::Literal(make_u8(1), LineInfo::default()),
            CheckedAst::Literal(make_u8(2), LineInfo::default()),
        );
        let result = interpret_ast(&ast, &mut std_env());
        assert!(result.is_ok());
        let result = result.unwrap();
        assert!(result.get_type().equals(&std_types::UINT8));
        assert_eq!(result, make_u8(3));
    }

    #[test]
    fn unit_function() {
        let ast = CheckedAst::Call {
            function: Box::new(fn_unit()),
            arg: Box::new(CheckedAst::Literal(Value::Unit, LineInfo::default())),
            return_type: std_types::UNIT,
            info: LineInfo::default(),
        };
        let result = interpret_ast(&ast, &mut global_env());
        assert!(result.is_ok());
        let result = result.unwrap();
        assert!(result.get_type().equals(&std_types::UNIT));
        assert_eq!(result, Value::Unit);
    }

    #[test]
    fn invalid_function() {
        let ast = CheckedAst::Call {
            function: Box::new(fn_unit()),
            arg: Box::new(CheckedAst::Literal(make_u8(1), LineInfo::default())),
            return_type: std_types::UNIT,
            info: LineInfo::default(),
        };
        let result = interpret_ast(&ast, &mut global_env());
        assert!(result.is_err());
    }

    #[test]
    fn assignment() {
        let ast = CheckedAst::Assignment(
            Box::new(CheckedAst::Identifier(
                "x".to_string(),
                std_types::UINT8,
                LineInfo::default(),
            )),
            Box::new(CheckedAst::Literal(make_u8(1), LineInfo::default())),
            std_types::UINT8,
            LineInfo::default(),
        );
        let result = interpret_ast(&ast, &mut std_env());
        assert!(result.is_ok());
        let result = result.unwrap();
        assert!(result.get_type().equals(&std_types::UINT8));
        assert_eq!(result, make_u8(1));
    }

    #[test]
    fn arithmetic_complex() {
        let result1 = parser::parse_str_one("8 / (1f32 / (3 * 3) - 1)", Some(&stdlib()))
            .expect("Failed to parse expression");
        let result2 = parser::parse_str_one("8 / (1.0 / (3 * 3) - 1)", Some(&stdlib()))
            .expect("Failed to parse expression");
        let mut checker = TypeChecker::default();
        stdlib().init_type_checker(&mut checker);
        let result1 = checker
            .check_module(&result1)
            .expect("Failed to type check expression");
        let result2 = checker
            .check_module(&result2)
            .expect("Failed to type check expression");
        let result1 = interpret_module(&result1, &mut std_env());
        let result2 = interpret_module(&result2, &mut std_env());
        assert!(result1.is_ok());
        assert!(result2.is_ok());
        let result1 = result1.unwrap();
        let result2 = result2.unwrap();
        assert!(result1.get_type().equals(&std_types::FLOAT32));
        assert!(result2.get_type().equals(&std_types::FLOAT32));
        assert_eq!(result1, make_f32(8.0 / (1.0 / (3.0 * 3.0) - 1.0)));
        assert_eq!(result2, make_f32(8.0 / (1.0 / (3.0 * 3.0) - 1.0)));
    }

    #[test]
    fn module_assign_add() {
        let module = parser::parse_str_all(
            r#"
			x = 1;
			y = 2;
			z = x + y;
		"#,
            None,
        )
        .expect("Failed to parse module");
        let mut checker = TypeChecker::default();
        let module = checker
            .check_module(&module)
            .expect("Failed to type check module");
        let result = interpret_module(&module, &mut std_env());
        assert!(result.is_ok());
        let result = result.unwrap();
        assert!(result.get_type().equals(&std_types::UINT8));
        assert_eq!(result, make_u8(3));
    }

    #[test]
    fn function_decl_paren_explicit_args_and_ret() {
        let module = parser::parse_str_all(
            r#"
			add(x: u8, y: u8, z: u8) -> u8 {
				x + y + z
			}
		"#,
            None,
        )
        .expect("Failed to parse module");
        let mut checker = TypeChecker::default();
        let module = checker
            .check_module(&module)
            .expect("Failed to type check module");
        let mut env = std_env();
        let result = interpret_module(&module, &mut env);
        assert!(result.is_ok());
        assert!(env.lookup_function("add").is_some());
    }

    #[test]
    fn function_decl_explicit_args_and_ret() {
        let module = parser::parse_str_all(
            r#"
			add x: u8 y: u8 z: u8 -> u8 {
				x + y + z
			}
		"#,
            None,
        )
        .expect("Failed to parse module");
        let mut checker = TypeChecker::default();
        let module = checker
            .check_module(&module)
            .expect("Failed to type check module");
        let mut env = std_env();
        let result = interpret_module(&module, &mut env);
        assert!(result.is_ok());
        assert!(env.lookup_function("add").is_some());
    }

    #[test]
    fn function_decl_explicit_args() {
        let module = parser::parse_str_all(
            r#"
			add x: u8 y: u8 z: u8 {
				x + y + z
			}
		"#,
            None,
        )
        .expect("Failed to parse module");
        let mut checker = TypeChecker::default();
        let module = checker
            .check_module(&module)
            .expect("Failed to type check module");
        let mut env = std_env();
        let result = interpret_module(&module, &mut env);
        assert!(result.is_ok());
        assert!(env.lookup_function("add").is_some());
    }

    #[test]
    fn function_decl_paren_implicit_args_and_ret() {
        let module = parser::parse_str_all(
            r#"
			add(x, y, z) {
				x + y + z
			}
		"#,
            None,
        )
        .expect("Failed to parse module");
        let mut checker = TypeChecker::default();
        let module = checker
            .check_module(&module)
            .expect("Failed to type check module");
        let mut env = std_env();
        let result = interpret_module(&module, &mut env);
        assert!(result.is_ok());
        assert!(env.lookup_function("add").is_some());
    }

    #[test]
    fn function_decl_implicit_args_and_ret() {
        let module = parser::parse_str_all(
            r#"
			add x y z {
				x + y + z
			}
		"#,
            None,
        )
        .expect("Failed to parse module");
        let mut checker = TypeChecker::default();
        let module = checker
            .check_module(&module)
            .expect("Failed to type check module");
        let mut env = std_env();
        let result = interpret_module(&module, &mut env);
        assert!(result.is_ok());
        assert!(env.lookup_function("add").is_some());
    }

    #[test]
    fn function_decl_implicit_random_parens() {
        let module = parser::parse_str_all(
            r#"
			add x y (z) a (b) (c) -> u8 {
				x + y + z + a + b + c
			}
		"#,
            None,
        )
        .expect("Failed to parse module");
        let mut checker = TypeChecker::default();
        let module = checker
            .check_module(&module)
            .expect("Failed to type check module");
        let mut env = std_env();
        let result = interpret_module(&module, &mut env);
        assert!(result.is_ok());
        assert!(env.lookup_function("add").is_some());
    }

    #[test]
    fn function_decl_paren_explicit_signature_oneline() {
        let module = parser::parse_str_all(
            r#"
			add(x: u8, y: u8, z: u8) -> u8 = x + y + z;
		"#,
            None,
        )
        .expect("Failed to parse module");
        let mut checker = TypeChecker::default();
        let module = checker
            .check_module(&module)
            .expect("Failed to type check module");
        let mut env = std_env();
        let result = interpret_module(&module, &mut env);
        assert!(result.is_ok());
        assert!(env.lookup_function("add").is_some());
    }

    #[test]
    fn function_decl_explicit_signature_oneline() {
        let module = parser::parse_str_all(
            r#"
			add x: u8 y: u8 z: u8 -> u8 = x + y + z;
		"#,
            None,
        )
        .expect("Failed to parse module");
        let mut checker = TypeChecker::default();
        let module = checker
            .check_module(&module)
            .expect("Failed to type check module");
        let mut env = std_env();
        let result = interpret_module(&module, &mut env);
        assert!(result.is_ok());
        assert!(env.lookup_function("add").is_some());
    }
}
