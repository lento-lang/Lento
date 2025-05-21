#[cfg(test)]
mod tests {

    use crate::{
        interpreter::{
            number::{Number, UnsignedInteger},
            value::{RecordKey, Value},
        },
        parser::{
            ast::{Ast, TypeAst},
            parser::from_string,
            pattern::BindPattern,
        },
        stdlib::init::{stdlib, Initializer},
        util::error::LineInfo,
    };

    fn make_u1(n: u8) -> Value {
        Value::Number(Number::UnsignedInteger(UnsignedInteger::UInt1(n)))
    }

    fn make_u8(n: u8) -> Value {
        Value::Number(Number::UnsignedInteger(UnsignedInteger::UInt8(n)))
    }

    fn lit(value: Value) -> Ast {
        Ast::Literal {
            value,
            info: LineInfo::default(),
        }
    }

    fn parse_str_one(
        input: &str,
        init: Option<&Initializer>,
    ) -> Result<Ast, crate::parser::error::ParseError> {
        let mut parser = from_string(input.to_string());
        if let Some(init) = init {
            init.init_parser(&mut parser);
        }
        parser.parse_one()
    }

    fn parse_str_all(
        input: &str,
        init: Option<&Initializer>,
    ) -> Result<Vec<Ast>, crate::parser::error::ParseError> {
        let mut parser = from_string(input.to_string());
        if let Some(init) = init {
            init.init_parser(&mut parser);
        }
        parser.parse_all()
    }

    #[test]
    fn number() {
        let result = parse_str_one("1", None);
        assert!(result.is_ok());
        let result = result.unwrap();

        assert!(result == lit(make_u1(1)));
    }

    #[test]
    fn number_many() {
        let result = parse_str_all("1 2 3 4 5", None);
        assert!(result.is_ok());
        let result = result.unwrap();
        assert!(result.len() == 5);
        assert!(result[0] == lit(make_u1(1)));
        assert!(result[1] == lit(make_u8(2)));
        assert!(result[2] == lit(make_u8(3)));
        assert!(result[3] == lit(make_u8(4)));
        assert!(result[4] == lit(make_u8(5)));
    }

    #[test]
    fn number_many_semicolon() {
        let result = parse_str_all("1; 2; 3;", None);
        assert!(result.is_ok());
        let result = result.unwrap();
        assert!(result.len() == 3);
        assert!(result[0] == lit(make_u1(1)));
        assert!(result[1] == lit(make_u8(2)));
        assert!(result[2] == lit(make_u8(3)));
    }

    #[test]
    fn number_par() {
        let result = parse_str_one("(1)", None);
        assert!(result.is_ok());
        let result = result.unwrap();

        assert!(result == lit(make_u1(1)));
    }

    #[test]
    fn tuple_1_trailing() {
        let result = parse_str_one("(1,)", None);
        assert!(result.is_ok());
        let result = result.unwrap();

        assert!(matches!(result, Ast::Tuple { .. }));
        if let Ast::Tuple { exprs, .. } = &result {
            assert_eq!(exprs.len(), 1);
            assert_eq!(exprs[0], lit(make_u1(1)));
        }
    }

    #[test]
    fn tuple_2() {
        let result = parse_str_one("(1, 2)", None);
        assert!(result.is_ok());
        let result = result.unwrap();

        assert!(matches!(result, Ast::Tuple { .. }));
        if let Ast::Tuple { exprs, .. } = &result {
            assert_eq!(exprs.len(), 2);
            assert_eq!(exprs[0], lit(make_u1(1)));
            assert_eq!(exprs[1], lit(make_u8(2)));
        }
    }

    #[test]
    fn tuple_3() {
        let result = parse_str_one("(1, 2, 3)", None);
        assert!(result.is_ok());
        let result = result.unwrap();

        assert!(matches!(result, Ast::Tuple { .. }));
        if let Ast::Tuple { exprs, .. } = &result {
            assert_eq!(exprs.len(), 3);
            assert_eq!(exprs[0], lit(make_u1(1)));
            assert_eq!(exprs[1], lit(make_u8(2)));
            assert_eq!(exprs[2], lit(make_u8(3)));
        }
    }

    #[test]
    fn tuple_3_trailing() {
        let result = parse_str_one("(1, 2, 3,)", None);
        assert!(result.is_ok());
        let result = result.unwrap();

        assert!(matches!(result, Ast::Tuple { .. }));
        if let Ast::Tuple { exprs, .. } = &result {
            assert_eq!(exprs.len(), 3);
            assert_eq!(exprs[0], lit(make_u1(1)));
            assert_eq!(exprs[1], lit(make_u8(2)));
            assert_eq!(exprs[2], lit(make_u8(3)));
        }
    }

    #[test]
    fn tuple_addition() {
        let result = parse_str_one("(1, 2) + (3, 4)", Some(&stdlib()));
        assert!(result.is_ok());
        let result = result.unwrap();

        assert!(matches!(result, Ast::Binary { .. }));
        if let Ast::Binary {
            lhs, op_info, rhs, ..
        } = &result
        {
            assert_eq!(&op_info.symbol, "+");
            assert!(matches!(*lhs.to_owned(), Ast::Tuple { .. }));
            assert!(matches!(*rhs.to_owned(), Ast::Tuple { .. }));
        }
    }

    #[test]
    fn list_3() {
        let result = parse_str_one("[1, 2, 3]", None);
        assert!(result.is_ok());
        let result = result.unwrap();

        assert!(matches!(result, Ast::List { .. }));
        if let Ast::List {
            exprs: elems,
            info: _,
        } = &result
        {
            assert_eq!(elems.len(), 3);
            assert_eq!(elems[0], lit(make_u1(1)));
            assert_eq!(elems[1], lit(make_u8(2)));
            assert_eq!(elems[2], lit(make_u8(3)));
        }
    }

    #[test]
    fn call_paren_apply() {
        let result = parse_str_one("print(\"Hello, World!\")", Some(&stdlib()));
        let expected = Ast::FunctionCall {
            expr: Box::new(Ast::Identifier {
                name: "print".to_string(),
                info: LineInfo::default(),
            }),
            arg: Box::new(lit(Value::String("Hello, World!".to_string()))),
            info: LineInfo::default(),
        };
        assert!(result.is_ok());
        let result = result.unwrap();

        assert!(result == expected);
    }

    #[test]
    fn call_no_paren_apply() {
        let result = parse_str_one("print \"Hello, World!\"", Some(&stdlib()));
        let expected = Ast::FunctionCall {
            expr: Box::new(Ast::Identifier {
                name: "print".to_string(),
                info: LineInfo::default(),
            }),
            arg: Box::new(lit(Value::String("Hello, World!".to_string()))),
            info: LineInfo::default(),
        };
        assert!(result.is_ok());
        let result = result.unwrap();

        assert!(result == expected);
    }

    #[test]
    fn call_tuple_apply() {
        let result = parse_str_one("println (\"Hello, World!\")", None);
        let expected = Ast::FunctionCall {
            expr: Box::new(Ast::Identifier {
                name: "println".to_string(),
                info: LineInfo::default(),
            }),
            arg: Box::new(lit(Value::String("Hello, World!".to_string()))),
            info: LineInfo::default(),
        };
        assert!(result.is_ok());
        let result = result.unwrap();

        assert!(result == expected);
    }

    #[test]
    fn hello_world_file() {
        let result = parse_str_all(
            include_str!("../../../examples/basic/hello_world.lt"),
            Some(&stdlib()),
        );
        let expected = Ast::FunctionCall {
            expr: Box::new(Ast::Identifier {
                name: "print".to_string(),
                info: LineInfo::default(),
            }),
            arg: Box::new(lit(Value::String("Hello, World!".to_string()))),
            info: LineInfo::default(),
        };
        assert!(result.is_ok());
        let result = result.unwrap();
        assert!(result.len() == 3);
        // All three should be the same
        assert!(result[0] == expected);
        assert!(result[1] == expected);
        assert!(result[2] == expected);
    }

    #[test]
    fn arithmetic() {
        let result = parse_str_one("1 + 2", Some(&stdlib()));
        assert!(result.is_ok());
        let result = result.unwrap();

        assert!(matches!(result, Ast::Binary { .. }));
        // Assert "add"
        if let Ast::Binary { op_info, .. } = &result {
            assert_eq!(&op_info.symbol, "+");
        }
        if let Ast::Binary { lhs, rhs, .. } = &result {
            // Always true
            assert!(matches!(
                *lhs.to_owned(),
                Ast::Literal {
                    value: Value::Number(_),
                    ..
                }
            ));
            assert!(matches!(
                *rhs.to_owned(),
                Ast::Literal {
                    value: Value::Number(_),
                    ..
                }
            ));
        }
    }

    #[test]
    fn arithmetic_tree() {
        let result = parse_str_one("1 + 2 + 3", Some(&stdlib()));
        assert!(result.is_ok());
        let result = result.unwrap();

        assert!(matches!(result, Ast::Binary { .. }));
        // Assert left side
        if let Ast::Binary { lhs, .. } = &result {
            assert!(matches!(**lhs, Ast::Binary { .. }));
        }
        // dbg!(&result.print_sexpr());
    }

    #[test]
    fn literal_type_identifier() {
        let result = parse_str_one("int", Some(&stdlib()));
        assert!(result.is_ok());
        let result = result.unwrap();
        assert!(matches!(result, Ast::LiteralType { .. }));
        if let Ast::LiteralType { expr, .. } = &result {
            assert!(matches!(expr, TypeAst::Identifier { .. }));
            let TypeAst::Identifier { name, .. } = &expr else {
                panic!("Expected identifier");
            };
            assert_eq!(name, "int");
        }
    }

    #[test]
    fn typed_assignment() {
        let result = parse_str_one("int x = 1", Some(&stdlib()));
        assert!(result.is_ok());
        let result = result.unwrap();
        println!("result: {:?}", result);

        assert!(matches!(result, Ast::Assignment { .. }));
        if let Ast::Assignment {
            target,
            expr,
            annotation,
            ..
        } = &result
        {
            assert!(matches!(target, BindPattern::Variable { .. }));
            assert!(matches!(*expr.to_owned(), Ast::Literal { .. }));
            assert!(annotation.to_owned().is_some());
            assert!(matches!(
                annotation.to_owned().unwrap(),
                TypeAst::Identifier { .. }
            ));
            if let Some(annotation) = annotation {
                assert!(matches!(annotation, TypeAst::Identifier { .. }));
                let TypeAst::Identifier { name, .. } = &annotation else {
                    panic!("Expected identifier");
                };
                assert_eq!(name, "int");
            }
        }
    }

    #[test]
    fn untyped_assignment() {
        let result = parse_str_one("x = 1", Some(&stdlib()));
        assert!(result.is_ok());
        let result = result.unwrap();
        println!("result: {:?}", result);

        assert!(matches!(result, Ast::Assignment { .. }));
    }

    #[test]
    fn assign_add() {
        let result = parse_str_one("x = 1 + 2", Some(&stdlib()));
        assert!(result.is_ok());
        let result = result.unwrap();
        println!("result: {:?}", result);

        assert!(matches!(result, Ast::Assignment { .. }));
        if let Ast::Assignment { target, expr, .. } = &result {
            assert!(matches!(target, BindPattern::Variable { .. }));
            assert!(matches!(*expr.to_owned(), Ast::Binary { .. }));
        }
    }

    #[test]
    fn comment() {
        let result = parse_str_all("1; // This is a comment", None);
        assert!(result.is_ok());
        let result = result.unwrap();
        assert!(result.len() == 1);
        assert!(matches!(result[0], Ast::Literal { .. }));
        assert!(result[0] == lit(make_u1(1)));
    }

    #[test]
    fn comment_newline() {
        let result = parse_str_all(
            r#"
			// This is a comment
			1; // This is a comment
			2;
			// This is a comment
		"#,
            None,
        );
        assert!(result.is_ok());
        let result = result.unwrap();
        assert!(result.len() == 2);
        assert!(result[0] == lit(make_u1(1)));
        assert!(result[1] == lit(make_u8(2)));
    }

    #[test]
    fn arithmetic_complex() {
        let result = parse_str_one("5 * (10 - 2) / 2 + 1", Some(&stdlib()));
        assert!(result.is_ok());
        let result = result.unwrap();

        assert!(matches!(result, Ast::Binary { .. }));
        if let Ast::Binary { lhs, rhs, .. } = &result {
            assert!(matches!(*lhs.to_owned(), Ast::Binary { .. }));
            assert!(matches!(*rhs.to_owned(), Ast::Literal { .. }));
            if let Ast::Binary { lhs, .. } = &**lhs {
                assert!(matches!(*lhs.to_owned(), Ast::Binary { .. }));
                assert!(matches!(*rhs.to_owned(), Ast::Literal { .. }));
                if let Ast::Binary { lhs, .. } = &**lhs {
                    assert!(matches!(*lhs.to_owned(), Ast::Literal { .. }));
                }
            }
        }
    }

    #[test]
    fn record_literal_empty() {
        let result = parse_str_one("{}", None);
        assert!(result.is_ok());
        let result = result.unwrap();

        assert!(matches!(result, Ast::Record { .. }));
        if let Ast::Record { fields, .. } = &result {
            assert_eq!(fields.len(), 0);
        }
    }

    #[test]
    fn record_literal_one() {
        let result = parse_str_one("{ x: 1 }", None);
        assert!(result.is_ok());
        let result = result.unwrap();

        assert!(matches!(result, Ast::Record { .. }));
        if let Ast::Record { fields, .. } = &result {
            assert_eq!(fields.len(), 1);
            let fields = fields.iter().collect::<Vec<_>>();
            assert!(matches!(fields[0].0, RecordKey::String(_)));
            if let RecordKey::String(key) = &fields[0].0 {
                assert_eq!(key, "x");
            }
            assert!(matches!(fields[0].1, Ast::Literal { .. }));
            assert_eq!(fields[0].1, lit(make_u1(1)));
        }
    }

    #[test]
    fn record_literal_two() {
        let result = parse_str_one("{ x: 1, y: 2 }", None);
        assert!(result.is_ok());
        let result = result.unwrap();

        assert!(matches!(result, Ast::Record { .. }));
        if let Ast::Record { fields, .. } = &result {
            assert_eq!(fields.len(), 2);
            let fields = fields.iter().collect::<Vec<_>>();
            assert!(matches!(fields[0].0, RecordKey::String(_)));
            assert!(matches!(fields[1].0, RecordKey::String(_)));
            if let RecordKey::String(key) = &fields[0].0 {
                assert_eq!(key, "x");
            }
            if let RecordKey::String(key) = &fields[1].0 {
                assert_eq!(key, "y");
            }
            assert!(matches!(fields[0].1, Ast::Literal { .. }));
            assert!(matches!(fields[1].1, Ast::Literal { .. }));
            assert_eq!(fields[0].1, lit(make_u1(1)));
            assert_eq!(fields[1].1, lit(make_u8(2)));
        }
    }

    #[test]
    fn record_literal_nested() {
        let result = parse_str_one("{ x: { y: 1 } }", None);
        assert!(result.is_ok());
        let result = result.unwrap();

        assert!(matches!(result, Ast::Record { .. }));
        if let Ast::Record { fields, .. } = &result {
            assert_eq!(fields.len(), 1);
            let fields = fields.iter().collect::<Vec<_>>();
            assert!(matches!(fields[0].0, RecordKey::String(_)));
            if let RecordKey::String(key) = &fields[0].0 {
                assert_eq!(key, "x");
            }
            assert!(matches!(fields[0].1, Ast::Record { .. }));
            if let Ast::Record {
                fields: inner_fields,
                ..
            } = &fields[0].1
            {
                assert_eq!(inner_fields.len(), 1);
                let inner_fields = inner_fields.iter().collect::<Vec<_>>();
                assert!(matches!(inner_fields[0].0, RecordKey::String(_)));
                if let RecordKey::String(key) = &inner_fields[0].0 {
                    assert_eq!(key, "y");
                }
                assert!(matches!(inner_fields[0].1, Ast::Literal { .. }));
                assert_eq!(inner_fields[0].1, lit(make_u1(1)));
            }
        }
    }

    #[test]
    fn record_nested_block() {
        let result = parse_str_one("{ x: { 1 + 2 } }", Some(&stdlib()));
        assert!(result.is_ok());
        let result = result.unwrap();

        assert!(matches!(result, Ast::Record { .. }));
        if let Ast::Record { fields, .. } = &result {
            assert_eq!(fields.len(), 1);
            let fields = fields.iter().collect::<Vec<_>>();
            assert!(matches!(fields[0].0, RecordKey::String(_)));
            if let RecordKey::String(key) = &fields[0].0 {
                assert_eq!(key, "x");
            }
            assert!(matches!(fields[0].1, Ast::Block { .. }));
            if let Ast::Block { exprs: inner, .. } = &fields[0].1 {
                assert_eq!(inner.len(), 1);
                assert!(matches!(inner[0], Ast::Binary { .. }));
            }
        }
    }

    #[test]
    fn block_one() {
        let result = parse_str_one("{ 1 }", None);
        assert!(result.is_ok());
        let result = result.unwrap();

        assert!(matches!(result, Ast::Block { .. }));
        if let Ast::Block { exprs: inner, .. } = &result {
            assert_eq!(inner.len(), 1);
            assert!(matches!(inner[0], Ast::Literal { .. }));
        }
    }

    #[test]
    fn block_two() {
        let result = parse_str_one("{ 1; 2 }", None);
        assert!(result.is_ok());
        let result = result.unwrap();

        assert!(matches!(result, Ast::Block { .. }));
        if let Ast::Block { exprs: inner, .. } = &result {
            assert_eq!(inner.len(), 2);
            assert!(matches!(inner[0], Ast::Literal { .. }));
            assert!(matches!(inner[1], Ast::Literal { .. }));
        }
    }

    #[test]
    fn block_three() {
        let result = parse_str_one("{ 1; 2; 3 }", None);
        assert!(result.is_ok());
        let result = result.unwrap();

        assert!(matches!(result, Ast::Block { .. }));
        if let Ast::Block { exprs: inner, .. } = &result {
            assert_eq!(inner.len(), 3);
            assert!(matches!(inner[0], Ast::Literal { .. }));
            assert!(matches!(inner[1], Ast::Literal { .. }));
            assert!(matches!(inner[2], Ast::Literal { .. }));
        }
    }

    #[test]
    fn block_three_no_semicolon() {
        let result = parse_str_one("{ 1 2 3 }", None);
        assert!(result.is_ok());
        let result = result.unwrap();

        assert!(matches!(result, Ast::Block { .. }));
        if let Ast::Block { exprs: inner, .. } = &result {
            assert_eq!(inner.len(), 3);
            assert!(matches!(inner[0], Ast::Literal { .. }));
            assert!(matches!(inner[1], Ast::Literal { .. }));
            assert!(matches!(inner[2], Ast::Literal { .. }));
        }
    }

    #[test]
    fn block_nested() {
        let result = parse_str_one("{ { 1 } }", None);
        assert!(result.is_ok());
        let result = result.unwrap();

        assert!(matches!(result, Ast::Block { .. }));
        if let Ast::Block { exprs: inner, .. } = &result {
            assert_eq!(inner.len(), 1);
            assert!(matches!(inner[0], Ast::Block { .. }));
        }
    }

    #[test]
    fn block_nested_two() {
        let result = parse_str_one("{ { 1; 2 } }", None);
        assert!(result.is_ok());
        let result = result.unwrap();

        assert!(matches!(result, Ast::Block { .. }));
        if let Ast::Block { exprs: inner, .. } = &result {
            assert_eq!(inner.len(), 1);
            assert!(matches!(inner[0], Ast::Block { .. }));
            if let Ast::Block {
                exprs: inner_inner, ..
            } = &inner[0]
            {
                assert_eq!(inner_inner.len(), 2);
                assert!(matches!(inner_inner[0], Ast::Literal { .. }));
                assert!(matches!(inner_inner[1], Ast::Literal { .. }));
            }
        }
    }

    #[test]
    fn function_def_paren_explicit_args_and_ret() {
        let result = parse_str_one("u8 add(u8 x, u8 y, u8 z) { x + y + z }", None);
        assert!(result.is_ok());
    }

    #[test]
    fn function_def_no_paren_explicit_args_and_ret() {
        let result = parse_str_one("u8 add u8 x, u8 y, u8 z { x + y + z }", None);
        assert!(result.is_ok());
    }

    #[test]
    fn function_def_no_paren_explicit_args() {
        let result = parse_str_one("add u8 x, u8 y, u8 z { x + y + z }", None);
        assert!(result.is_ok());
    }

    #[test]
    fn function_def_paren_implicit_args_and_ret() {
        let result = parse_str_one("add(x, y, z) { x + y + z }", None);
        assert!(result.is_ok());
    }

    #[test]
    fn function_def_no_paren_implicit_args_and_ret() {
        let result = parse_str_one("add x, y, z { x + y + z }", None);
        assert!(result.is_ok());
    }

    #[test]
    fn function_def_mixed_parens() {
        let result = parse_str_one(
            "u8 add x, y, (z), a, (b), (c) { x + y + z + a + b + c }",
            None,
        );
        assert!(result.is_ok());
    }

    #[test]
    fn function_def_paren_explicit_oneline() {
        let result = parse_str_one("u8 add(u8 x, u8 y, u8 z) = x + y + z;", None);
        assert!(result.is_ok());
    }

    #[test]
    fn function_def_no_paren_explicit_oneline() {
        let result = parse_str_one("u8 add u8 x, u8 y, u8 z = x + y + z;", None);
        assert!(result.is_ok());
    }

    #[test]
    fn function_def_paren_implicit_oneline() {
        let result = parse_str_one("add(x, y, z) = x + y + z;", None);
        assert!(result.is_ok());
    }

    #[test]
    fn function_def_no_paren_implicit_oneline() {
        let result = parse_str_one("add x, y, z = x + y + z;", None);
        assert!(result.is_ok());
    }

    #[test]
    fn function_def_with_return_type() {
        let result = parse_str_one("int add(int x, int y) = x + y;", None);
        assert!(result.is_ok());
    }

    #[test]
    fn function_def_with_return_type_no_parens() {
        let result = parse_str_one("int add int x, int y = x + y;", None);
        assert!(result.is_ok());
    }

    #[test]
    fn function_def_with_return_type_block() {
        let result = parse_str_one("int add(int x, int y) { x + y }", None);
        assert!(result.is_ok());
    }

    #[test]
    fn function_def_with_return_type_no_parens_block() {
        let result = parse_str_one("int add int x, int y { x + y }", None);
        assert!(result.is_ok());
    }

    #[test]
    fn function_def_multiple_statements() {
        let result = parse_str_one(
            "int add(int x, int y) {
                let z = x + y;
                z
            }",
            None,
        );
        assert!(result.is_ok());
    }

    #[test]
    fn function_def_nested() {
        let result = parse_str_one(
            "int outer(int x) {
                int inner(int y) = x + y;
                inner(x)
            }",
            None,
        );
        assert!(result.is_ok());
    }

    #[test]
    fn parse_assignment_with_type() {
        let result = parse_str_one("int x = 123", Some(&stdlib()));
        assert!(result.is_ok());
        if let Ast::Assignment {
            target,
            expr,
            annotation,
            ..
        } = result.unwrap()
        {
            assert!(matches!(target, BindPattern::Variable { .. }));
            assert!(matches!(*expr, Ast::Literal { .. }));
            assert!(annotation.is_some());
        } else {
            panic!("Expected assignment");
        }
    }

    #[test]
    fn parse_function_def_with_type_and_paren_arg() {
        let result = parse_str_one("int f(int x) = x + 5", Some(&stdlib()));
        assert!(result.is_ok());
    }

    #[test]
    fn parse_function_def_with_paren_arg() {
        let result = parse_str_one("f(int x) = x + 5", Some(&stdlib()));
        assert!(result.is_ok());
    }

    #[test]
    fn parse_function_def_with_type_and_parenless_arg() {
        let result = parse_str_one("int f(x) = x + 5", Some(&stdlib()));
        assert!(result.is_ok());
    }

    #[test]
    fn parse_function_def_with_parenless_arg() {
        let result = parse_str_one("f(x) = x + 5", Some(&stdlib()));
        assert!(result.is_ok());
    }

    #[test]
    fn parse_function_def_with_type_and_explicit_arg() {
        let result = parse_str_one("int f int x = x + 5", Some(&stdlib()));
        assert!(result.is_ok());
    }

    #[test]
    fn parse_function_def_with_explicit_arg() {
        let result = parse_str_one("f x = x + 5", Some(&stdlib()));
        assert!(result.is_ok());
    }

    #[test]
    fn parse_function_def_with_multiple_explicit_args() {
        let result = parse_str_one("f int x, int y = x + y", Some(&stdlib()));
        assert!(result.is_ok());
    }

    #[test]
    fn parse_function_def_with_type_and_paren_args_block() {
        let result = parse_str_one(
            "int f(int x, int y) {
					x + y
				}",
            Some(&stdlib()),
        );
        assert!(result.is_ok());
    }

    #[test]
    fn parse_function_def_with_type_and_explicit_args_block() {
        let result = parse_str_one(
            "int f int x, int y {
					x + y
				}",
            Some(&stdlib()),
        );
        assert!(result.is_ok());
    }

    #[test]
    fn parse_function_def_with_type_and_paren_args_oneline() {
        let result = parse_str_one("int f(int x, int y) = x + y;", Some(&stdlib()));
        assert!(result.is_ok());
    }

    #[test]
    fn parse_function_def_with_type_and_explicit_args_multiline() {
        let result = parse_str_one(
            "int f
				  int x,
				  int y
				  = x + y;",
            Some(&stdlib()),
        );
        assert!(result.is_ok());
    }
}
