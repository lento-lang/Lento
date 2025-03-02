#[cfg(test)]
mod tests {
    use crate::{
        interpreter::{
            env::{global_env, Environment},
            eval::{eval_expr, eval_exprs},
            number::{FloatingPoint, Number, UnsignedInteger},
            value::Value,
        },
        util::error::LineInfo,
        parser::parser::{self, ParseResult, ParseResults},
        stdlib::init::{stdlib, Initializer},
        type_checker::{
            checked_ast::{CheckedAst, CheckedParam},
            checker::TypeChecker,
            types::{std_types, GetType, Type, TypeTrait},
        },
    };

    fn parse_str_one(expr: &str, std: Option<&Initializer>) -> ParseResult {
        let mut parser = parser::from_string(expr.into());
        if let Some(std) = std {
            std.init_parser(&mut parser);
        }
        parser.parse_one()
    }

    fn parse_str_all(expr: &str, std: Option<&Initializer>) -> ParseResults {
        let mut parser = parser::from_string(expr.into());
        if let Some(std) = std {
            std.init_parser(&mut parser);
        }
        parser.parse_all()
    }

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
        CheckedAst::FunctionCall {
            expr: Box::new(CheckedAst::FunctionCall {
                expr: Box::new(CheckedAst::Identifier {
                    name: "add".into(),
                    ty: std_types::NUM(),
                    info: LineInfo::default(),
                }),
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
        CheckedAst::function_def(
            CheckedParam {
                name: "ignore".to_string(),
                ty: std_types::UNIT,
            },
            CheckedAst::Block {
                exprs: vec![],
                ty: std_types::UNIT,
                info: LineInfo::default(),
            },
            std_types::UNIT,
            LineInfo::default(),
        )
    }

    #[test]
    fn binary_add() {
        let ast = add(
            CheckedAst::Literal {
                value: make_u8(1),
                info: LineInfo::default(),
            },
            CheckedAst::Literal {
                value: make_u8(2),
                info: LineInfo::default(),
            },
        );
        let result = eval_expr(&ast, &mut std_env());
        assert!(result.is_ok());
        let result = result.unwrap();
        assert!(result.get_type().equals(&std_types::UINT8).success);
        assert_eq!(result, make_u8(3));
    }

    #[test]
    fn tuple() {
        let ast = CheckedAst::Tuple {
            exprs: vec![
                CheckedAst::Literal {
                    value: make_u8(1),
                    info: LineInfo::default(),
                },
                CheckedAst::Literal {
                    value: make_u8(2),
                    info: LineInfo::default(),
                },
                CheckedAst::Literal {
                    value: make_u8(3),
                    info: LineInfo::default(),
                },
            ],
            expr_types: Type::Tuple(vec![std_types::UINT8; 3]),
            info: LineInfo::default(),
        };
        let result = eval_expr(&ast, &mut std_env());
        assert!(result.is_ok());
        let result = result.unwrap();
        assert!(
            result
                .get_type()
                .equals(&Type::Tuple(vec![std_types::UINT8; 3]))
                .success
        );
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
            CheckedAst::Literal {
                value: make_u8(1),
                info: LineInfo::default(),
            },
            CheckedAst::Literal {
                value: make_u8(2),
                info: LineInfo::default(),
            },
        );
        let result = eval_expr(&ast, &mut std_env());
        assert!(result.is_ok());
        let result = result.unwrap();
        assert!(result.get_type().equals(&std_types::UINT8).success);
        assert_eq!(result, make_u8(3));
    }

    #[test]
    fn unit_function() {
        let ast = CheckedAst::FunctionCall {
            expr: Box::new(fn_unit()),
            arg: Box::new(CheckedAst::Literal {
                value: Value::Unit,
                info: LineInfo::default(),
            }),
            return_type: std_types::UNIT,
            info: LineInfo::default(),
        };
        let result = eval_expr(&ast, &mut global_env());
        assert!(result.is_ok());
        let result = result.unwrap();
        assert!(result.get_type().equals(&std_types::UNIT).success);
        assert_eq!(result, Value::Unit);
    }

    #[test]
    fn invalid_function() {
        let ast = CheckedAst::FunctionCall {
            expr: Box::new(fn_unit()),
            arg: Box::new(CheckedAst::Literal {
                value: make_u8(1),
                info: LineInfo::default(),
            }),
            return_type: std_types::UNIT,
            info: LineInfo::default(),
        };
        let result = eval_expr(&ast, &mut global_env());
        assert!(result.is_err());
    }

    #[test]
    fn assignment() {
        let ast = CheckedAst::Assignment {
            target: Box::new(CheckedAst::Identifier {
                name: "x".to_string(),
                ty: std_types::UINT8,
                info: LineInfo::default(),
            }),
            expr: Box::new(CheckedAst::Literal {
                value: make_u8(1),
                info: LineInfo::default(),
            }),
            ty: std_types::UINT8,
            info: LineInfo::default(),
        };
        let result = eval_expr(&ast, &mut std_env());
        assert!(result.is_ok());
        let result = result.unwrap();
        assert!(result.get_type().equals(&std_types::UINT8).success);
        assert_eq!(result, make_u8(1));
    }

    #[test]
    fn arithmetic_complex() {
        let result1 = parse_str_one("8 / (1f32 / (3 * 3) - 1)", Some(&stdlib()))
            .expect("Failed to parse expression");
        let result2 = parse_str_one("8 / (1.0 / (3 * 3) - 1)", Some(&stdlib()))
            .expect("Failed to parse expression");
        let mut checker = TypeChecker::default();
        stdlib().init_type_checker(&mut checker);
        let result1 = checker
            .check_expr(&result1)
            .expect("Failed to type check expression");
        let result2 = checker
            .check_expr(&result2)
            .expect("Failed to type check expression");
        let result1 = eval_expr(&result1, &mut std_env());
        let result2 = eval_expr(&result2, &mut std_env());
        assert!(result1.is_ok());
        assert!(result2.is_ok());
        let result1 = result1.unwrap();
        let result2 = result2.unwrap();
        assert!(result1.get_type().equals(&std_types::FLOAT32).success);
        assert!(result2.get_type().equals(&std_types::FLOAT32).success);
        assert_eq!(result1, make_f32(8.0 / (1.0 / (3.0 * 3.0) - 1.0)));
        assert_eq!(result2, make_f32(8.0 / (1.0 / (3.0 * 3.0) - 1.0)));
    }

    #[test]
    fn module_assign_add() {
        let module = parse_str_all(
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
            .check_top_exprs(&module)
            .expect("Failed to type check module");
        let result = eval_exprs(&module, &mut std_env());
        assert!(result.is_ok());
        let result = result.unwrap();
        assert!(result.get_type().equals(&std_types::UINT8).success);
        assert_eq!(result, make_u8(3));
    }

    #[test]
    fn function_decl_paren_explicit_args_and_ret() {
        let module = parse_str_all(
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
            .check_top_exprs(&module)
            .expect("Failed to type check module");
        let mut env = std_env();
        let result = eval_exprs(&module, &mut env);
        assert!(result.is_ok());
        assert!(env.lookup_function("add").is_some());
    }

    #[test]
    fn function_decl_explicit_args_and_ret() {
        let module = parse_str_all(
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
            .check_top_exprs(&module)
            .expect("Failed to type check module");
        let mut env = std_env();
        let result = eval_exprs(&module, &mut env);
        assert!(result.is_ok());
        assert!(env.lookup_function("add").is_some());
    }

    #[test]
    fn function_decl_explicit_args() {
        let module = parse_str_all(
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
            .check_top_exprs(&module)
            .expect("Failed to type check module");
        let mut env = std_env();
        let result = eval_exprs(&module, &mut env);
        assert!(result.is_ok());
        assert!(env.lookup_function("add").is_some());
    }

    #[test]
    fn function_decl_paren_implicit_args_and_ret() {
        let module = parse_str_all(
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
            .check_top_exprs(&module)
            .expect("Failed to type check module");
        let mut env = std_env();
        let result = eval_exprs(&module, &mut env);
        assert!(result.is_ok());
        assert!(env.lookup_function("add").is_some());
    }

    #[test]
    fn function_decl_implicit_args_and_ret() {
        let module = parse_str_all(
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
            .check_top_exprs(&module)
            .expect("Failed to type check module");
        let mut env = std_env();
        let result = eval_exprs(&module, &mut env);
        assert!(result.is_ok());
        assert!(env.lookup_function("add").is_some());
    }

    #[test]
    fn function_decl_implicit_random_parens() {
        let module = parse_str_all(
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
            .check_top_exprs(&module)
            .expect("Failed to type check module");
        let mut env = std_env();
        let result = eval_exprs(&module, &mut env);
        assert!(result.is_ok());
        assert!(env.lookup_function("add").is_some());
    }

    #[test]
    fn function_decl_paren_explicit_signature_oneline() {
        let module = parse_str_all(
            r#"
			add(x: u8, y: u8, z: u8) -> u8 = x + y + z;
		"#,
            None,
        )
        .expect("Failed to parse module");
        let mut checker = TypeChecker::default();
        let module = checker
            .check_top_exprs(&module)
            .expect("Failed to type check module");
        let mut env = std_env();
        let result = eval_exprs(&module, &mut env);
        assert!(result.is_ok());
        assert!(env.lookup_function("add").is_some());
    }

    #[test]
    fn function_decl_explicit_signature_oneline() {
        let module = parse_str_all(
            r#"
			add x: u8 y: u8 z: u8 -> u8 = x + y + z;
		"#,
            None,
        )
        .expect("Failed to parse module");
        let mut checker = TypeChecker::default();
        let module = checker
            .check_top_exprs(&module)
            .expect("Failed to type check module");
        let mut env = std_env();
        let result = eval_exprs(&module, &mut env);
        assert!(result.is_ok());
        assert!(env.lookup_function("add").is_some());
    }
}
