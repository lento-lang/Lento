#[cfg(test)]
mod tests {
    use crate::{
        interpreter::{
            number::{Number, UnsignedInteger},
            value::{RecordKey, Value},
        },
        parser::{ast::Ast, parser::from_string, pattern::BindPattern},
        stdlib::init::{stdlib, Initializer},
        type_checker::checked_ast::{ArrayLenAst, TypeAst},
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
    fn unit() {
        let result = parse_str_one("()", None);
        let result = result.unwrap();

        assert!(matches!(result, Ast::Tuple { .. }));
        if let Ast::Tuple { exprs, info: _ } = &result {
            assert_eq!(exprs.len(), 0);
        }
    }

    #[test]
    fn number() {
        let result = parse_str_one("1", None);
        let result = result.unwrap();

        assert!(result == lit(make_u1(1)));
    }

    #[test]
    fn number_many() {
        let result = parse_str_all("1 \n 2 \n 3 \n 4 \n 5", None);
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
        let result = result.unwrap();
        assert!(result.len() == 3);
        assert!(result[0] == lit(make_u1(1)));
        assert!(result[1] == lit(make_u8(2)));
        assert!(result[2] == lit(make_u8(3)));
    }

    #[test]
    fn number_par() {
        let result = parse_str_one("(1)", None);
        let result = result.unwrap();

        assert!(result == lit(make_u1(1)));
    }

    #[test]
    fn tuple_2() {
        let result = parse_str_one("(1, 2)", None);
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
        let result = result.unwrap();

        assert!(matches!(result, Ast::Binary { .. }));
        if let Ast::Binary {
            lhs,
            op: op_info,
            rhs,
            ..
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
        let result = result.unwrap();

        assert!(result == expected);
    }

    #[test]
    fn hello_world_file() {
        let result = parse_str_all(
            include_str!("../../../../examples/basic/hello_world.lt"),
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
        let result = result.unwrap();

        assert!(matches!(result, Ast::Binary { .. }));
        // Assert "add"
        if let Ast::Binary { op: op_info, .. } = &result {
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
        let result = parse_str_one("int", None);
        let result = result.unwrap();
        assert!(matches!(result, Ast::Identifier { .. }));
        if let Ast::Identifier { name, .. } = &result {
            assert_eq!(name, "int");
        } else {
            panic!("Expected identifier");
        };
    }

    #[test]
    fn untyped_assignment() {
        let result = parse_str_one("x = 1", Some(&stdlib()));
        let result = result.unwrap();

        assert!(matches!(result, Ast::Let { .. }));
    }

    #[test]
    fn assign_add() {
        let result = parse_str_one("x = 1 + 2", Some(&stdlib()));
        let result = result.unwrap();

        assert!(matches!(result, Ast::Let { .. }));
        if let Ast::Let { target, expr, .. } = &result {
            assert!(matches!(target, BindPattern::Variable { .. }));
            assert!(matches!(*expr.to_owned(), Ast::Binary { .. }));
        }
    }

    #[test]
    fn comment() {
        let result = parse_str_all("1; // This is a comment", None);
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
        let result = result.unwrap();
        assert!(result.len() == 2);
        assert!(result[0] == lit(make_u1(1)));
        assert!(result[1] == lit(make_u8(2)));
    }

    #[test]
    fn arithmetic_complex() {
        let result = parse_str_one("5 * (10 - 2) / 2 + 1", Some(&stdlib()));
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
        let result = result.unwrap();

        assert!(matches!(result, Ast::Record { .. }));
        if let Ast::Record { fields, .. } = &result {
            assert_eq!(fields.len(), 0);
        }
    }

    #[test]
    fn record_literal_one() {
        let result = parse_str_one("{ x: 1 }", None);
        let result = result.unwrap();

        assert!(matches!(result, Ast::Record { .. }));
        if let Ast::Record { fields, .. } = &result {
            assert_eq!(fields.len(), 1);
            let fields = fields.iter().collect::<Vec<_>>();
            assert!(matches!(fields[0].0, RecordKey::String(_)));
            let RecordKey::String(key) = &fields[0].0;
            assert_eq!(key, "x");
            assert!(matches!(fields[0].1, Ast::Literal { .. }));
            assert_eq!(fields[0].1, lit(make_u1(1)));
        }
    }

    #[test]
    fn record_literal_two() {
        let result = parse_str_one("{ x: 1, y: 2 }", None);
        let result = result.unwrap();

        assert!(matches!(result, Ast::Record { .. }));
        if let Ast::Record { fields, .. } = &result {
            assert_eq!(fields.len(), 2);
            let fields = fields.iter().collect::<Vec<_>>();
            assert!(matches!(fields[0].0, RecordKey::String(_)));
            assert!(matches!(fields[1].0, RecordKey::String(_)));
            let RecordKey::String(key) = &fields[0].0;
            assert_eq!(key, "x");
            let RecordKey::String(key) = &fields[1].0;
            assert_eq!(key, "y");
            assert!(matches!(fields[0].1, Ast::Literal { .. }));
            assert!(matches!(fields[1].1, Ast::Literal { .. }));
            assert_eq!(fields[0].1, lit(make_u1(1)));
            assert_eq!(fields[1].1, lit(make_u8(2)));
        }
    }

    #[test]
    fn record_literal_nested() {
        let result = parse_str_one("{ x: { y: 1 } }", None);
        let result = result.unwrap();

        assert!(matches!(result, Ast::Record { .. }));
        if let Ast::Record { fields, .. } = &result {
            assert_eq!(fields.len(), 1);
            let fields = fields.iter().collect::<Vec<_>>();
            assert!(matches!(fields[0].0, RecordKey::String(_)));
            let RecordKey::String(key) = &fields[0].0;
            assert_eq!(key, "x");
            assert!(matches!(fields[0].1, Ast::Record { .. }));
            if let Ast::Record {
                fields: inner_fields,
                ..
            } = &fields[0].1
            {
                assert_eq!(inner_fields.len(), 1);
                let inner_fields = inner_fields.iter().collect::<Vec<_>>();
                assert!(matches!(inner_fields[0].0, RecordKey::String(_)));
                let RecordKey::String(key) = &inner_fields[0].0;
                assert_eq!(key, "y");
                assert!(matches!(inner_fields[0].1, Ast::Literal { .. }));
                assert_eq!(inner_fields[0].1, lit(make_u1(1)));
            }
        }
    }

    #[test]
    fn record_nested_block() {
        let result = parse_str_one("{ x: { 1 + 2 } }", Some(&stdlib()));
        let result = result.unwrap();

        assert!(matches!(result, Ast::Record { .. }));
        if let Ast::Record { fields, .. } = &result {
            assert_eq!(fields.len(), 1);
            let fields = fields.iter().collect::<Vec<_>>();
            assert!(matches!(fields[0].0, RecordKey::String(_)));
            let RecordKey::String(key) = &fields[0].0;
            assert_eq!(key, "x");
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
        let result = parse_str_one("{ 1 \n 2 \n 3 }", None);
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
    #[ignore = "legacy function-definition syntax"]
    fn function_def_paren_explicit_args_and_ret() {
        parse_str_one("u8 add(u8 x, u8 y, u8 z) = { x + y + z }", Some(&stdlib())).unwrap();
    }

    #[test]
    #[ignore = "legacy function-definition syntax"]
    fn function_def_no_paren_explicit_args_and_ret() {
        parse_str_one("u8 add u8 x, u8 y, u8 z = { x + y + z }", Some(&stdlib())).unwrap();
    }

    #[test]
    #[ignore = "legacy function-definition syntax"]
    fn function_def_no_paren_explicit_args() {
        parse_str_one("add u8 x, u8 y, u8 z = { x + y + z }", Some(&stdlib())).unwrap();
    }

    #[test]
    #[ignore = "legacy function-definition syntax"]
    fn function_def_paren_implicit_args_and_ret() {
        parse_str_one("add(x, y, z) = { x + y + z }", Some(&stdlib())).unwrap();
    }

    #[test]
    #[ignore = "legacy function-definition syntax"]
    fn function_def_no_paren_implicit_args_and_ret() {
        parse_str_one("add x, y, z = { x + y + z }", Some(&stdlib())).unwrap();
    }

    #[test]
    #[ignore = "legacy function-definition syntax"]
    fn function_def_mixed_parens() {
        parse_str_one(
            "u8 add x, y, (z), a, (b), (c) = { x + y + z + a + b + c }",
            Some(&stdlib()),
        )
        .unwrap();
    }

    #[test]
    #[ignore = "legacy function-definition syntax"]
    fn function_def_paren_explicit_oneline() {
        parse_str_one("u8 add(u8 x, u8 y, u8 z) = x + y + z;", Some(&stdlib())).unwrap();
    }

    #[test]
    #[ignore = "legacy function-definition syntax"]
    fn function_def_no_paren_explicit_oneline() {
        parse_str_one("u8 add u8 x, u8 y, u8 z = x + y + z;", Some(&stdlib())).unwrap();
    }

    #[test]
    #[ignore = "legacy function-definition syntax"]
    fn function_def_paren_implicit_oneline() {
        parse_str_one("add(x, y, z) = x + y + z;", Some(&stdlib())).unwrap();
    }

    #[test]
    #[ignore = "legacy function-definition syntax"]
    fn function_def_no_paren_implicit_oneline() {
        parse_str_one("add x, y, z = x + y + z;", Some(&stdlib())).unwrap();
    }

    #[test]
    #[ignore = "legacy function-definition syntax"]
    fn function_def_with_return_type() {
        parse_str_one("int add(int x, int y) = x + y;", Some(&stdlib())).unwrap();
    }

    #[test]
    #[ignore = "legacy function-definition syntax"]
    fn function_def_with_return_type_no_parens() {
        parse_str_one("int add int x, int y = x + y;", Some(&stdlib())).unwrap();
    }

    #[test]
    #[ignore = "legacy function-definition syntax"]
    fn function_def_with_return_type_block() {
        parse_str_one("int add(int x, int y) = { x + y }", Some(&stdlib())).unwrap();
    }

    #[test]
    #[ignore = "legacy function-definition syntax"]
    fn function_def_multiple_statements() {
        parse_str_one(
            "int add(int x, int y) = {
                let z = x + y;
                z
            }",
            Some(&stdlib()),
        )
        .unwrap();
    }

    #[test]
    #[ignore = "legacy function-definition syntax"]
    fn function_def_nested() {
        parse_str_one(
            "int outer(int x) = {
                int inner(int y) = x + y;
                inner(x)
            }",
            Some(&stdlib()),
        )
        .unwrap();
    }

    #[test]
    #[ignore = "legacy function-definition syntax"]
    fn function_def_with_type_and_paren_arg() {
        let result = parse_str_one("int f(int x) = x + 5", Some(&stdlib()));
        if let Ast::Let { target, expr, .. } = result.unwrap() {
            // assert!(annotation.is_some());
            // if let Some(TypeAst::Identifier { name, .. }) = annotation {
            //     assert_eq!(name, "int");
            // }
            assert!(matches!(target, BindPattern::Variable { .. }));
            if let BindPattern::Variable { name, .. } = target {
                assert_eq!(name, "f");
            }
            assert!(matches!(*expr, Ast::Lambda { .. }));
            if let Ast::Lambda {  body, .. } = *expr {
                // assert!(param.ty.is_some());
                // if let Some(TypeAst::Identifier { name, .. }) = param.ty {
                //     assert_eq!(name, "int");
                // }
                // if let BindPattern::Variable { name, .. } = &param {
                //     assert_eq!(name, "x");
                // }
                assert!(matches!(*body, Ast::Binary { .. }));
                if let Ast::Binary { lhs, rhs, .. } = *body {
                    assert!(matches!(*lhs, Ast::Identifier { .. }));
                    if let Ast::Identifier { name, .. } = *lhs {
                        assert_eq!(name, "x");
                    }
                    assert!(matches!(*rhs, Ast::Literal { .. }));
                    if let Ast::Literal { value, .. } = *rhs {
                        assert_eq!(
                            value,
                            Value::Number(Number::UnsignedInteger(UnsignedInteger::UInt8(5)))
                        );
                    }
                }
            }
        } else {
            panic!("Expected function definition");
        }
    }

    #[test]
    #[ignore = "legacy function-definition syntax"]
    fn function_def_with_paren_arg() {
        let result = parse_str_one("f(int x) = x + 5", Some(&stdlib()));
        if let Ast::Let { target, expr, .. } = result.unwrap() {
            assert!(matches!(target, BindPattern::Variable { .. }));
            if let BindPattern::Variable { name, .. } = target {
                assert_eq!(name, "f");
            }
            assert!(matches!(*expr, Ast::Lambda { .. }));
            if let Ast::Lambda {  body, .. } = *expr {
                // assert!(param.ty.is_some());
                // if let Some(TypeAst::Identifier { name, .. }) = param.ty {
                //     assert_eq!(name, "int");
                // }
                // if let BindPattern::Variable { name, .. } = &param.pattern {
                //     assert_eq!(name, "x");
                // }
                assert!(matches!(*body, Ast::Binary { .. }));
                if let Ast::Binary { lhs, rhs, .. } = *body {
                    assert!(matches!(*lhs, Ast::Identifier { .. }));
                    if let Ast::Identifier { name, .. } = *lhs {
                        assert_eq!(name, "x");
                    }
                    assert!(matches!(*rhs, Ast::Literal { .. }));
                    if let Ast::Literal { value, .. } = *rhs {
                        assert_eq!(
                            value,
                            Value::Number(Number::UnsignedInteger(UnsignedInteger::UInt8(5)))
                        );
                    }
                }
            }
        } else {
            panic!("Expected function definition");
        }
    }

    #[test]
    #[ignore = "legacy function-definition syntax"]
    fn function_def_with_type_and_parenless_arg() {
        let result = parse_str_one("int f(x) = x + 5", Some(&stdlib()));
        if let Ast::Let { target, expr, .. } = result.unwrap() {
            // assert!(annotation.is_some());
            // if let Some(TypeAst::Identifier { name, .. }) = annotation {
            //     assert_eq!(name, "int");
            // }
            assert!(matches!(target, BindPattern::Variable { .. }));
            if let BindPattern::Variable { name, .. } = target {
                assert_eq!(name, "f");
            }
            assert!(matches!(*expr, Ast::Lambda { .. }));
            if let Ast::Lambda {  body, .. } = *expr {
                // assert!(param.ty.is_none());
                // if let BindPattern::Variable { name, .. } = &param.pattern {
                //     assert_eq!(name, "x");
                // }
                assert!(matches!(*body, Ast::Binary { .. }));
                if let Ast::Binary { lhs, rhs, .. } = *body {
                    assert!(matches!(*lhs, Ast::Identifier { .. }));
                    if let Ast::Identifier { name, .. } = *lhs {
                        assert_eq!(name, "x");
                    }
                    assert!(matches!(*rhs, Ast::Literal { .. }));
                    if let Ast::Literal { value, .. } = *rhs {
                        assert_eq!(
                            value,
                            Value::Number(Number::UnsignedInteger(UnsignedInteger::UInt8(5)))
                        );
                    }
                }
            }
        } else {
            panic!("Expected function definition");
        }
    }

    #[test]
    #[ignore = "legacy function-definition syntax"]
    fn function_def_with_parenless_arg() {
        let result = parse_str_one("f(x) = x + 5", Some(&stdlib()));
        if let Ast::Let { target, expr, .. } = result.unwrap() {
            assert!(matches!(target, BindPattern::Variable { .. }));
            if let BindPattern::Variable { name, .. } = target {
                assert_eq!(name, "f");
            }
            assert!(matches!(*expr, Ast::Lambda { .. }));
            if let Ast::Lambda {  body, .. } = *expr {
                // assert!(param.ty.is_none());
                // if let BindPattern::Variable { name, .. } = &param.pattern {
                //     assert_eq!(name, "x");
                // }
                assert!(matches!(*body, Ast::Binary { .. }));
                if let Ast::Binary { lhs, rhs, .. } = *body {
                    assert!(matches!(*lhs, Ast::Identifier { .. }));
                    if let Ast::Identifier { name, .. } = *lhs {
                        assert_eq!(name, "x");
                    }
                    assert!(matches!(*rhs, Ast::Literal { .. }));
                    if let Ast::Literal { value, .. } = *rhs {
                        assert_eq!(
                            value,
                            Value::Number(Number::UnsignedInteger(UnsignedInteger::UInt8(5)))
                        );
                    }
                }
            }
        } else {
            panic!("Expected function definition");
        }
    }

    #[test]
    #[ignore = "legacy function-definition syntax"]
    fn function_def_with_type_and_explicit_arg() {
        let result = parse_str_one("int f int x = x + 5", Some(&stdlib()));
        if let Ast::Let { target, expr, .. } = result.unwrap() {
            assert!(matches!(target, BindPattern::Variable { .. }));
            if let BindPattern::Variable { name, .. } = target {
                assert_eq!(name, "f");
            }
            // assert!(annotation.is_some());
            // if let Some(TypeAst::Identifier { name, .. }) = annotation {
            //     assert_eq!(name, "int");
            // }
            assert!(matches!(*expr, Ast::Lambda { .. }));
            if let Ast::Lambda {  body, .. } = *expr {
                // assert!(param.ty.is_some());
                // if let Some(TypeAst::Identifier { name, .. }) = param.ty {
                //     assert_eq!(name, "int");
                // }
                // if let BindPattern::Variable { name, .. } = &param.pattern {
                //     assert_eq!(name, "x");
                // }
                assert!(matches!(*body, Ast::Binary { .. }));
                if let Ast::Binary { lhs, rhs, .. } = *body {
                    assert!(matches!(*lhs, Ast::Identifier { .. }));
                    if let Ast::Identifier { name, .. } = *lhs {
                        assert_eq!(name, "x");
                    }
                    assert!(matches!(*rhs, Ast::Literal { .. }));
                    if let Ast::Literal { value, .. } = *rhs {
                        assert_eq!(
                            value,
                            Value::Number(Number::UnsignedInteger(UnsignedInteger::UInt8(5)))
                        );
                    }
                }
            }
        } else {
            panic!("Expected function definition");
        }
    }

    #[test]
    #[ignore = "legacy function-definition syntax"]
    fn function_def_with_explicit_arg() {
        let result = parse_str_one("f x = x + 5", Some(&stdlib()));
        if let Ast::Let { target, expr, .. } = result.unwrap() {
            assert!(matches!(target, BindPattern::Variable { .. }));
            if let BindPattern::Variable { name, .. } = target {
                assert_eq!(name, "f");
            }
            assert!(matches!(*expr, Ast::Lambda { .. }));
            if let Ast::Lambda {  body, .. } = *expr {
                // assert!(param.ty.is_none());
                // if let BindPattern::Variable { name, .. } = &param.pattern {
                //     assert_eq!(name, "x");
                // }
                assert!(matches!(*body, Ast::Binary { .. }));
                if let Ast::Binary { lhs, rhs, .. } = *body {
                    assert!(matches!(*lhs, Ast::Identifier { .. }));
                    if let Ast::Identifier { name, .. } = *lhs {
                        assert_eq!(name, "x");
                    }
                    assert!(matches!(*rhs, Ast::Literal { .. }));
                    if let Ast::Literal { value, .. } = *rhs {
                        assert_eq!(
                            value,
                            Value::Number(Number::UnsignedInteger(UnsignedInteger::UInt8(5)))
                        );
                    }
                }
            }
        } else {
            panic!("Expected function definition");
        }
    }

    #[test]
    #[ignore = "legacy function-definition syntax"]
    fn function_def_with_multiple_explicit_args() {
        let result = parse_str_one("f int x, int y = x + y", Some(&stdlib()));
        if let Ast::Let { target, expr, .. } = result.unwrap() {
            assert!(matches!(target, BindPattern::Variable { .. }));
            if let BindPattern::Variable { name, .. } = target {
                assert_eq!(name, "f");
            }
            assert!(matches!(*expr, Ast::Lambda { .. }));
            if let Ast::Lambda {  body, .. } = *expr {
                // assert!(param.ty.is_some());
                // if let Some(TypeAst::Identifier { name, .. }) = param.ty {
                //     assert_eq!(name, "int");
                // }
                // if let BindPattern::Variable { name, .. } = &param.pattern {
                //     assert_eq!(name, "x");
                // }
                assert!(matches!(*body, Ast::Lambda { .. }));
                if let Ast::Lambda {  body, .. } = *body {
                    // assert!(param.ty.is_some());
                    // if let Some(TypeAst::Identifier { name, .. }) = param.ty {
                    //     assert_eq!(name, "int");
                    // }
                    // if let BindPattern::Variable { name, .. } = &param.pattern {
                    //     assert_eq!(name, "y");
                    // }
                    assert!(matches!(*body, Ast::Binary { .. }));
                    if let Ast::Binary { lhs, rhs, .. } = *body {
                        assert!(matches!(*lhs, Ast::Identifier { .. }));
                        if let Ast::Identifier { name, .. } = *lhs {
                            assert_eq!(name, "x");
                        }
                        assert!(matches!(*rhs, Ast::Identifier { .. }));
                        if let Ast::Identifier { name, .. } = *rhs {
                            assert_eq!(name, "y");
                        }
                    }
                }
            }
        } else {
            panic!("Expected function definition");
        }
    }

    #[test]
    #[ignore = "legacy function-definition syntax"]
    fn function_def_with_type_and_paren_args_block() {
        let result = parse_str_one(
            "int f(int x, int y) = {
                    x + y
                }",
            Some(&stdlib()),
        );
        if let Ast::Let { target, expr, .. } = result.unwrap() {
            // assert!(annotation.is_some());
            // if let Some(TypeAst::Identifier { name, .. }) = annotation {
            //     assert_eq!(name, "int");
            // }
            assert!(matches!(target, BindPattern::Variable { .. }));
            if let BindPattern::Variable { name, .. } = target {
                assert_eq!(name, "f");
            }
            assert!(matches!(*expr, Ast::Lambda { .. }));
            if let Ast::Lambda {  body, .. } = *expr {
                // assert!(param.ty.is_some());
                // if let Some(TypeAst::Identifier { name, .. }) = param.ty {
                //     assert_eq!(name, "int");
                // }
                // if let BindPattern::Variable { name, .. } = &param.pattern {
                //     assert_eq!(name, "x");
                // }
                assert!(matches!(*body, Ast::Lambda { .. }));
                if let Ast::Lambda {  body, .. } = *body {
                    // assert!(param.ty.is_some());
                    // if let Some(TypeAst::Identifier { name, .. }) = param.ty {
                    //     assert_eq!(name, "int");
                    // }
                    // if let BindPattern::Variable { name, .. } = &param.pattern {
                    //     assert_eq!(name, "y");
                    // }
                    assert!(matches!(*body, Ast::Block { .. }));
                    if let Ast::Block { exprs, .. } = *body {
                        assert_eq!(exprs.len(), 1);
                        assert!(matches!(exprs[0], Ast::Binary { .. }));
                        if let Ast::Binary { lhs, rhs, .. } = &exprs[0] {
                            assert!(matches!(**lhs, Ast::Identifier { .. }));
                            if let Ast::Identifier { ref name, .. } = **lhs {
                                assert_eq!(name, "x");
                            }
                            assert!(matches!(**rhs, Ast::Identifier { .. }));
                            if let Ast::Identifier { ref name, .. } = **rhs {
                                assert_eq!(name, "y");
                            }
                        }
                    }
                }
            }
        } else {
            panic!("Expected function definition");
        }
    }

    #[test]
    #[ignore = "legacy function-definition syntax"]
    fn function_def_with_type_and_explicit_args_block() {
        let result = parse_str_one(
            "int f int x, int y = {
                    x + y
                }",
            Some(&stdlib()),
        );
        if let Ast::Let { target, expr, .. } = result.unwrap() {
            assert!(matches!(target, BindPattern::Variable { .. }));
            if let BindPattern::Variable { name, .. } = target {
                assert_eq!(name, "f");
            }
            // assert!(annotation.is_some());
            // if let Some(TypeAst::Identifier { name, .. }) = annotation {
            //     assert_eq!(name, "int");
            // }
            assert!(matches!(*expr, Ast::Lambda { .. }));
            if let Ast::Lambda {  body, .. } = *expr {
                // assert!(param.ty.is_some());
                // if let Some(TypeAst::Identifier { name, .. }) = param.ty {
                //     assert_eq!(name, "int");
                // }
                // if let BindPattern::Variable { name, .. } = &param.pattern {
                //     assert_eq!(name, "x");
                // }
                assert!(matches!(*body, Ast::Lambda { .. }));
                if let Ast::Lambda {  body, .. } = *body {
                    // assert!(param.ty.is_some());
                    // if let Some(TypeAst::Identifier { name, .. }) = param.ty {
                    //     assert_eq!(name, "int");
                    // }
                    // if let BindPattern::Variable { name, .. } = &param.pattern {
                    //     assert_eq!(name, "y");
                    // }
                    assert!(matches!(*body, Ast::Block { .. }));
                    if let Ast::Block { exprs, .. } = *body {
                        assert_eq!(exprs.len(), 1);
                        assert!(matches!(exprs[0], Ast::Binary { .. }));
                        if let Ast::Binary { lhs, rhs, .. } = &exprs[0] {
                            assert!(matches!(**lhs, Ast::Identifier { .. }));
                            if let Ast::Identifier { ref name, .. } = **lhs {
                                assert_eq!(name, "x");
                            }
                            assert!(matches!(**rhs, Ast::Identifier { .. }));
                            if let Ast::Identifier { ref name, .. } = **rhs {
                                assert_eq!(name, "y");
                            }
                        }
                    }
                }
            }
        } else {
            panic!("Expected function definition");
        }
    }

    #[test]
    #[ignore = "legacy function-definition syntax"]
    fn function_def_with_type_and_paren_args_oneline() {
        let result = parse_str_one("int f(int x, int y) = x + y;", Some(&stdlib()));
        if let Ast::Let { target, expr, .. } = result.unwrap() {
            // assert!(annotation.is_some());
            // if let Some(TypeAst::Identifier { name, .. }) = annotation {
            //     assert_eq!(name, "int");
            // }
            assert!(matches!(target, BindPattern::Variable { .. }));
            if let BindPattern::Variable { name, .. } = target {
                assert_eq!(name, "f");
            }
            assert!(matches!(*expr, Ast::Lambda { .. }));
            if let Ast::Lambda {  body, .. } = *expr {
                // assert!(param.ty.is_some());
                // if let Some(TypeAst::Identifier { name, .. }) = param.ty {
                //     assert_eq!(name, "int");
                // }
                // if let BindPattern::Variable { name, .. } = &param.pattern {
                //     assert_eq!(name, "x");
                // }
                assert!(matches!(*body, Ast::Lambda { .. }));
                if let Ast::Lambda {  body, .. } = *body {
                    // assert!(param.ty.is_some());
                    // if let Some(TypeAst::Identifier { name, .. }) = param.ty {
                    //     assert_eq!(name, "int");
                    // }
                    // if let BindPattern::Variable { name, .. } = &param.pattern {
                    //     assert_eq!(name, "y");
                    // }
                    assert!(matches!(*body, Ast::Binary { .. }));
                    if let Ast::Binary { lhs, rhs, .. } = *body {
                        assert!(matches!(*lhs, Ast::Identifier { .. }));
                        if let Ast::Identifier { name, .. } = *lhs {
                            assert_eq!(name, "x");
                        }
                        assert!(matches!(*rhs, Ast::Identifier { .. }));
                        if let Ast::Identifier { name, .. } = *rhs {
                            assert_eq!(name, "y");
                        }
                    }
                }
            }
        } else {
            panic!("Expected function definition");
        }
    }

    #[test]
    #[ignore = "legacy function-definition syntax"]
    fn function_def_with_type_and_explicit_args_multiline() {
        let result = parse_str_one(
            "int f
                  int x,
                  int y
                  = x + y;",
            Some(&stdlib()),
        );
        let result = result.unwrap();
        if let Ast::Let { target, expr, .. } = result {
            // assert!(annotation.is_some());
            // if let Some(TypeAst::Identifier { name, .. }) = annotation {
            //     assert_eq!(name, "int");
            // }
            assert!(matches!(target, BindPattern::Variable { .. }));
            if let BindPattern::Variable { name, .. } = target {
                assert_eq!(name, "f");
            }
            assert!(matches!(*expr, Ast::Lambda { .. }));
            if let Ast::Lambda {  body, .. } = *expr {
                // assert!(param.ty.is_some());
                // if let Some(TypeAst::Identifier { name, .. }) = param.ty {
                //     assert_eq!(name, "int");
                // }
                // if let BindPattern::Variable { name, .. } = &param.pattern {
                //     assert_eq!(name, "x");
                // }
                assert!(matches!(*body, Ast::Lambda { .. }));
                if let Ast::Lambda {  body, .. } = *body {
                    // assert!(param.ty.is_some());
                    // if let Some(TypeAst::Identifier { name, .. }) = param.ty {
                    //     assert_eq!(name, "int");
                    // }
                    // if let BindPattern::Variable { name, .. } = &param.pattern {
                    //     assert_eq!(name, "y");
                    // }
                    assert!(matches!(*body, Ast::Binary { .. }));
                    if let Ast::Binary { lhs, rhs, .. } = *body {
                        assert!(matches!(*lhs, Ast::Identifier { .. }));
                        if let Ast::Identifier { name, .. } = *lhs {
                            assert_eq!(name, "x");
                        }
                        assert!(matches!(*rhs, Ast::Identifier { .. }));
                        if let Ast::Identifier { name, .. } = *rhs {
                            assert_eq!(name, "y");
                        }
                    }
                }
            }
        } else {
            dbg!(result);
            panic!("Expected function definition");
        }
    }

    #[test]
    fn type_decl_simple() {
        let result = parse_str_one("type Foo = u8", None).unwrap();
        if let Ast::TypeDecl { name, params, .. } = result {
            assert_eq!(name, "Foo");
            assert!(params.is_empty());
        } else {
            panic!("Expected type declaration");
        }
    }

    #[test]
    fn type_decl_union() {
        let result = parse_str_one("type X = int | bool", Some(&stdlib())).unwrap();
        if let Ast::TypeDecl { body, .. } = result {
            assert!(matches!(body, TypeAst::Sum { .. }));
            if let TypeAst::Sum { variants, .. } = body {
                assert_eq!(variants.len(), 2);
            }
        } else {
            panic!("Expected type declaration");
        }
    }

    #[test]
    fn type_decl_function_type() {
        let result = parse_str_one("type Mapper = int -> bool", Some(&stdlib())).unwrap();
        if let Ast::TypeDecl { body, .. } = result {
            assert!(matches!(body, TypeAst::Lambda { .. }));
            if let TypeAst::Lambda { eff, .. } = body {
                assert!(eff.is_empty());
            }
        } else {
            panic!("Expected type declaration");
        }
    }

    #[test]
    fn type_decl_function_type_with_effect() {
        let result = parse_str_one("type Mapper = int -> bool ! io", Some(&stdlib())).unwrap();
        if let Ast::TypeDecl { body, .. } = result {
            assert!(matches!(body, TypeAst::Lambda { .. }));
            if let TypeAst::Lambda { eff, .. } = body {
                assert!(!eff.is_empty());
            }
        } else {
            panic!("Expected type declaration");
        }
    }

    #[test]
    fn type_decl_application_paren_args() {
        let result = parse_str_one("type Dict = Map(int, int)", Some(&stdlib())).unwrap();
        if let Ast::TypeDecl { body, .. } = result {
            assert!(matches!(body, TypeAst::Application { .. }));
            if let TypeAst::Application { expr, args, .. } = body {
                assert!(matches!(*expr, TypeAst::Identifier { .. }));
                assert_eq!(args.len(), 2);
            }
        } else {
            panic!("Expected type declaration");
        }
    }

    #[test]
    fn type_decl_application_juxtaposition_args() {
        let result = parse_str_one("type Dict = Map int int", Some(&stdlib())).unwrap();
        if let Ast::TypeDecl { body, .. } = result {
            assert!(matches!(body, TypeAst::Application { .. }));
            if let TypeAst::Application { args, .. } = body {
                assert_eq!(args.len(), 2);
            }
        } else {
            panic!("Expected type declaration");
        }
    }

    #[test]
    fn type_decl_tuple_type() {
        let result = parse_str_one("type Pair = (int, bool)", Some(&stdlib())).unwrap();
        if let Ast::TypeDecl { body, .. } = result {
            assert!(matches!(body, TypeAst::Tuple { .. }));
            if let TypeAst::Tuple { items, .. } = body {
                assert_eq!(items.len(), 2);
            }
        } else {
            panic!("Expected type declaration");
        }
    }

    #[test]
    fn type_decl_singleton_literal() {
        let result = parse_str_one("type FortyTwo = 42", Some(&stdlib())).unwrap();
        if let Ast::TypeDecl { body, .. } = result {
            assert!(matches!(body, TypeAst::Literal { .. }));
        } else {
            panic!("Expected type declaration");
        }
    }

    #[test]
    fn type_decl_record_type() {
        let result =
            parse_str_one("type Eq = { eq: Self -> Self -> bool }", Some(&stdlib())).unwrap();
        if let Ast::TypeDecl { body, .. } = result {
            assert!(matches!(body, TypeAst::Record { .. }));
        } else {
            panic!("Expected type declaration");
        }
    }

    #[test]
    fn type_decl_refinement_type() {
        let result = parse_str_one("type Nat = { v: int | v >= 0 }", Some(&stdlib())).unwrap();
        if let Ast::TypeDecl { body, .. } = result {
            assert!(matches!(body, TypeAst::Refinement { .. }));
        } else {
            panic!("Expected type declaration");
        }
    }

    #[test]
    fn type_decl_static_vec_generic_bare_param() {
        let result = parse_str_one("type MyArr T = [T; 6]", Some(&stdlib())).unwrap();
        if let Ast::TypeDecl { params, body, .. } = result {
            assert_eq!(params.len(), 1);
            assert!(matches!(body, TypeAst::Array { .. }));
            if let TypeAst::Array { len, .. } = body {
                assert_eq!(len, ArrayLenAst::Known(6));
            }
        } else {
            panic!("Expected type declaration");
        }
    }

    #[test]
    fn type_decl_static_vec_symbolic_len() {
        let result = parse_str_one("type MyArr(n: int, T: Type) = [T; n]", Some(&stdlib())).unwrap();
        if let Ast::TypeDecl { body, .. } = result {
            if let TypeAst::Array { len, .. } = body {
                assert_eq!(len, ArrayLenAst::Symbol("n".to_string()));
            } else {
                panic!("Expected array type declaration body");
            }
        } else {
            panic!("Expected type declaration");
        }
    }

    #[test]
    fn let_member_access_target_parses() {
        let result = parse_str_one("let (Eq int).eq = prim_int_eq", Some(&stdlib())).unwrap();
        if let Ast::Let { target, .. } = result {
            assert!(matches!(target, BindPattern::MemberAccess { .. }));
        } else {
            panic!("Expected let binding");
        }
    }

    #[test]
    fn qualified_function_name_parses() {
        let result = parse_str_one(
            "fn (Eq List T).eq<T: Eq>(xs, ys) = list_eq_by(T.eq, xs, ys)",
            Some(&stdlib()),
        )
        .unwrap();
        if let Ast::FunctionDef { name, params, .. } = result {
            assert!(name.contains(".eq"));
            assert_eq!(params.len(), 2);
        } else {
            panic!("Expected function definition");
        }
    }

    #[test]
    fn type_decl_list_type() {
        let result = parse_str_one("type Foo = [int]", Some(&stdlib())).unwrap();
        if let Ast::TypeDecl { body, .. } = result {
            assert!(matches!(body, TypeAst::List { .. }));
            if let TypeAst::List { elem, .. } = &body {
                assert!(matches!(elem.as_ref(), TypeAst::Identifier { name, .. } if name == "int"));
            }
        } else {
            panic!("Expected type declaration");
        }
    }

    #[test]
    fn type_decl_apply_bare_and_paren() {
        let result = parse_str_one("type A = MyArr X", Some(&stdlib())).unwrap();
        if let Ast::TypeDecl { body, .. } = result {
            assert!(matches!(body, TypeAst::Application { .. }));
        } else {
            panic!("Expected type declaration");
        }

        let result = parse_str_one("type B = MyArr(X)", Some(&stdlib())).unwrap();
        if let Ast::TypeDecl { body, .. } = result {
            assert!(matches!(body, TypeAst::Application { .. }));
        } else {
            panic!("Expected type declaration");
        }
    }

    #[test]
    fn function_def_bind_pattern_params() {
        let result = parse_str_one("fn f((x, y), { a: a }) = x", None).unwrap();
        if let Ast::FunctionDef { params, .. } = result {
            assert_eq!(params.len(), 2);
            assert!(matches!(params[0].0, BindPattern::Tuple { .. }));
            assert!(matches!(params[1].0, BindPattern::Record { .. }));
        } else {
            panic!("Expected function definition");
        }
    }

    #[test]
    fn let_keyword_decl() {
        let result = parse_str_one("let x = 1", None).unwrap();
        assert!(matches!(result, Ast::Let { .. }));
    }

    #[test]
    fn let_with_type_annotation() {
        let result = parse_str_one("let x: int = 5", Some(&stdlib())).unwrap();
        if let Ast::Let { target, annotation, .. } = result {
            assert!(matches!(target, BindPattern::Variable { .. }));
            assert!(annotation.is_some());
            assert!(matches!(annotation.unwrap(), TypeAst::Identifier { name, .. } if name == "int"));
        } else {
            panic!("Expected let binding");
        }
    }

    #[test]
    fn let_with_list_type_annotation() {
        let result = parse_str_one("let xs: [int] = [1, 2, 3]", Some(&stdlib())).unwrap();
        if let Ast::Let { annotation, .. } = result {
            assert!(annotation.is_some());
            assert!(matches!(annotation.unwrap(), TypeAst::List { .. }));
        } else {
            panic!("Expected let binding");
        }
    }

    #[test]
    fn let_with_tuple_type_annotation() {
        let result =
            parse_str_one("let pair: (int, bool) = (1, true)", Some(&stdlib())).unwrap();
        if let Ast::Let { annotation, .. } = result {
            assert!(annotation.is_some());
            assert!(matches!(annotation.unwrap(), TypeAst::Tuple { .. }));
        } else {
            panic!("Expected let binding");
        }
    }

    #[test]
    fn function_def_typed_params_parse() {
        let result = parse_str_one("fn id(x: u8) = x", Some(&stdlib())).unwrap();
        if let Ast::FunctionDef { params, .. } = result {
            assert_eq!(params.len(), 1);
            assert!(matches!(params[0].0, BindPattern::Variable { .. }));
            assert!(params[0].1.is_some());
        } else {
            panic!("Expected function definition");
        }
    }

    #[test]
    fn type_decl_literal_sum() {
        let result = parse_str_one("type FewNums = 5 | 6 | 7", Some(&stdlib())).unwrap();
        if let Ast::TypeDecl { body, .. } = result {
            assert!(matches!(body, TypeAst::Sum { .. }));
            if let TypeAst::Sum { variants, .. } = body {
                assert_eq!(variants.len(), 3);
                for variant in &variants {
                    assert!(matches!(variant, TypeAst::Literal { .. }));
                }
            }
        } else {
            panic!("Expected type declaration");
        }
    }

    #[test]
    fn let_with_literal_sum_annotation() {
        let result = parse_str_one("let x: \"hello\" | false = \"hello\"", Some(&stdlib())).unwrap();
        if let Ast::Let { annotation, .. } = result {
            let ann = annotation.unwrap();
            assert!(matches!(ann, TypeAst::Sum { .. }));
            if let TypeAst::Sum { variants, .. } = ann {
                assert_eq!(variants.len(), 2);
                assert!(matches!(&variants[0], TypeAst::Literal { .. }));
                assert!(matches!(&variants[1], TypeAst::Literal { .. }));
            }
        } else {
            panic!("Expected let binding");
        }
    }
}
