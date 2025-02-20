#[cfg(test)]
mod tests {
    use std::path::Path;

    use crate::{
        interpreter::{
            number::{Number, UnsignedInteger},
            value::{RecordKey, Value},
        },
        parser::{
            ast::Ast,
            parser::{parse_path_one, parse_str_all, parse_str_one},
        },
        stdlib::init::stdlib,
    };

    fn make_u1(n: u8) -> Value {
        Value::Number(Number::UnsignedInteger(UnsignedInteger::UInt1(n)))
    }

    fn make_u8(n: u8) -> Value {
        Value::Number(Number::UnsignedInteger(UnsignedInteger::UInt8(n)))
    }

    #[test]
    fn number() {
        let result = parse_str_one("1", None);
        assert!(result.is_ok());
        let result = result.unwrap();
        assert!(result.expressions.len() == 1);
        assert!(result.expressions[0] == Ast::Literal(make_u1(1)));
    }

    #[test]
    fn number_many() {
        let result = parse_str_all("1 2 3 4 5", None);
        assert!(result.is_ok());
        let result = result.unwrap();
        assert!(result.expressions.len() == 5);
        assert!(result.expressions[0] == Ast::Literal(make_u1(1)));
        assert!(result.expressions[1] == Ast::Literal(make_u8(2)));
        assert!(result.expressions[2] == Ast::Literal(make_u8(3)));
        assert!(result.expressions[3] == Ast::Literal(make_u8(4)));
        assert!(result.expressions[4] == Ast::Literal(make_u8(5)));
    }

    #[test]
    fn number_many_semicolon() {
        let result = parse_str_all("1; 2; 3;", None);
        assert!(result.is_ok());
        let result = result.unwrap();
        assert!(result.expressions.len() == 3);
        assert!(result.expressions[0] == Ast::Literal(make_u1(1)));
        assert!(result.expressions[1] == Ast::Literal(make_u8(2)));
        assert!(result.expressions[2] == Ast::Literal(make_u8(3)));
    }

    #[test]
    fn number_par() {
        let result = parse_str_one("(1)", None);
        assert!(result.is_ok());
        let result = result.unwrap();
        assert!(result.expressions.len() == 1);
        assert!(result.expressions[0] == Ast::Literal(make_u1(1)));
    }

    #[test]
    fn tuple_1_trailing() {
        let result = parse_str_one("(1,)", None);
        assert!(result.is_ok());
        let result = result.unwrap();
        assert!(result.expressions.len() == 1);
        assert!(matches!(result.expressions[0], Ast::Tuple(_)));
        if let Ast::Tuple(elems) = &result.expressions[0] {
            assert_eq!(elems.len(), 1);
            assert_eq!(elems[0], Ast::Literal(make_u1(1)));
        }
    }

    #[test]
    fn tuple_2() {
        let result = parse_str_one("(1, 2)", None);
        assert!(result.is_ok());
        let result = result.unwrap();
        assert!(result.expressions.len() == 1);
        assert!(matches!(result.expressions[0], Ast::Tuple(_)));
        if let Ast::Tuple(elems) = &result.expressions[0] {
            assert_eq!(elems.len(), 2);
            assert_eq!(elems[0], Ast::Literal(make_u1(1)));
            assert_eq!(elems[1], Ast::Literal(make_u8(2)));
        }
    }

    #[test]
    fn tuple_3() {
        let result = parse_str_one("(1, 2, 3)", None);
        assert!(result.is_ok());
        let result = result.unwrap();
        assert!(result.expressions.len() == 1);
        assert!(matches!(result.expressions[0], Ast::Tuple(_)));
        if let Ast::Tuple(elems) = &result.expressions[0] {
            assert_eq!(elems.len(), 3);
            assert_eq!(elems[0], Ast::Literal(make_u1(1)));
            assert_eq!(elems[1], Ast::Literal(make_u8(2)));
            assert_eq!(elems[2], Ast::Literal(make_u8(3)));
        }
    }

    #[test]
    fn tuple_3_trailing() {
        let result = parse_str_one("(1, 2, 3,)", None);
        assert!(result.is_ok());
        let result = result.unwrap();
        assert!(result.expressions.len() == 1);
        assert!(matches!(result.expressions[0], Ast::Tuple(_)));
        if let Ast::Tuple(elems) = &result.expressions[0] {
            assert_eq!(elems.len(), 3);
            assert_eq!(elems[0], Ast::Literal(make_u1(1)));
            assert_eq!(elems[1], Ast::Literal(make_u8(2)));
            assert_eq!(elems[2], Ast::Literal(make_u8(3)));
        }
    }

    #[test]
    fn tuple_addition() {
        let result = parse_str_one("(1, 2) + (3, 4)", None);
        assert!(result.is_ok());
        let result = result.unwrap();
        assert!(result.expressions.len() == 1);
        assert!(matches!(result.expressions[0], Ast::Binary(_, _, _)));
        if let Ast::Binary(lhs, op, rhs) = &result.expressions[0] {
            assert_eq!(op.name, "add".to_string());
            assert!(matches!(*lhs.to_owned(), Ast::Tuple(_)));
            assert!(matches!(*rhs.to_owned(), Ast::Tuple(_)));
        }
    }

    #[test]
    fn list_3() {
        let result = parse_str_one("[1, 2, 3]", None);
        assert!(result.is_ok());
        let result = result.unwrap();
        assert!(result.expressions.len() == 1);
        assert!(matches!(result.expressions[0], Ast::List(_)));
        if let Ast::List(elems) = &result.expressions[0] {
            assert_eq!(elems.len(), 3);
            assert_eq!(elems[0], Ast::Literal(make_u1(1)));
            assert_eq!(elems[1], Ast::Literal(make_u8(2)));
            assert_eq!(elems[2], Ast::Literal(make_u8(3)));
        }
    }

    #[test]
    fn call_paren_apply() {
        let result = parse_str_one("println(\"Hello, World!\")", None);
        let expected = Ast::Call(
            Box::new(Ast::Identifier("println".to_string())),
            Box::new(Ast::Literal(Value::String("Hello, World!".to_string()))),
        );
        assert!(result.is_ok());
        let result = result.unwrap();
        assert!(result.expressions.len() == 1);
        assert!(result.expressions[0] == expected);
    }

    #[test]
    fn call_no_paren_apply() {
        let result = parse_str_one("println \"Hello, World!\"", None);
        let expected = Ast::Call(
            Box::new(Ast::Identifier("println".to_string())),
            Box::new(Ast::Literal(Value::String("Hello, World!".to_string()))),
        );
        assert!(result.is_ok());
        let result = result.unwrap();
        assert!(result.expressions.len() == 1);
        assert!(result.expressions[0] == expected);
    }

    #[test]
    fn call_tuple_apply() {
        let result = parse_str_one("println (\"Hello, World!\")", None);
        let expected = Ast::Call(
            Box::new(Ast::Identifier("println".to_string())),
            Box::new(Ast::Literal(Value::String("Hello, World!".to_string()))),
        );
        assert!(result.is_ok());
        let result = result.unwrap();
        assert!(result.expressions.len() == 1);
        assert!(result.expressions[0] == expected);
    }

    #[test]
    fn hello_world_file() {
        let result = parse_path_one(Path::new("./examples/basic/hello_world.lt"), None);
        let expected = Ast::Call(
            Box::new(Ast::Identifier("println".to_string())),
            Box::new(Ast::Literal(Value::String("Hello, World!".to_string()))),
        );
        assert!(result.is_ok());
        let result = result.unwrap();
        assert!(result.expressions.len() == 1);
        assert!(result.expressions[0] == expected);
    }

    #[test]
    fn arithmetic() {
        let result = parse_str_one("1 + 2", None);
        assert!(result.is_ok());
        let result = result.unwrap();
        assert!(result.expressions.len() == 1);
        assert!(matches!(result.expressions[0], Ast::Binary(_, _, _)));
        // Assert "add"
        if let Ast::Binary(_, op, _) = &result.expressions[0] {
            assert_eq!(op.name, "add".to_string());
        }
        if let Ast::Binary(lhs, _, rhs) = &result.expressions[0] {
            // Always true
            assert!(matches!(*lhs.to_owned(), Ast::Literal(Value::Number(_))));
            assert!(matches!(*rhs.to_owned(), Ast::Literal(Value::Number(_))));
        }
    }

    #[test]
    fn arithmetic_tree() {
        let result = parse_str_one("1 + 2 + 3", None);
        assert!(result.is_ok());
        let result = result.unwrap();
        assert!(result.expressions.len() == 1);
        assert!(matches!(result.expressions[0], Ast::Binary(_, _, _)));
        // Assert left side
        if let Ast::Binary(lhs, _, _) = &result.expressions[0] {
            assert!(matches!(**lhs, Ast::Binary(_, _, _)));
        }
        // dbg!(&result.expressions[0].print_sexpr());
    }

    #[test]
    fn assignment() {
        let result = parse_str_one("x = 1", None);
        assert!(result.is_ok());
        let result = result.unwrap();
        assert!(result.expressions.len() == 1);
        assert!(matches!(result.expressions[0], Ast::Assignment(_, _)));
    }

    #[test]
    fn assign_add() {
        let result = parse_str_one("x = 1 + 2", None);
        assert!(result.is_ok());
        let result = result.unwrap();
        assert!(result.expressions.len() == 1);
        assert!(matches!(result.expressions[0], Ast::Assignment(_, _)));
        if let Ast::Assignment(lhs, rhs) = &result.expressions[0] {
            assert!(matches!(*lhs.to_owned(), Ast::Identifier(_)));
            assert!(matches!(*rhs.to_owned(), Ast::Binary(_, _, _)));
        }
    }

    #[test]
    fn comment() {
        let result = parse_str_all("1; // This is a comment", None);
        assert!(result.is_ok());
        let result = result.unwrap();
        assert!(result.expressions.len() == 1);
        assert!(result.expressions[0] == Ast::Literal(make_u1(1)));
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
        assert!(result.expressions.len() == 2);
        assert!(result.expressions[0] == Ast::Literal(make_u1(1)));
        assert!(result.expressions[1] == Ast::Literal(make_u8(2)));
    }

    #[test]
    fn arithmetic_complex() {
        let result = parse_str_one("5 * (10 - 2) / 2 + 1", Some(&stdlib()));
        assert!(result.is_ok());
        let result = result.unwrap();
        assert!(result.expressions.len() == 1);
        assert!(matches!(result.expressions[0], Ast::Binary(_, _, _)));
        if let Ast::Binary(lhs, _, rhs) = &result.expressions[0] {
            assert!(matches!(*lhs.to_owned(), Ast::Binary(_, _, _)));
            assert!(matches!(*rhs.to_owned(), Ast::Literal(_)));
            if let Ast::Binary(lhs, _, _) = &**lhs {
                assert!(matches!(*lhs.to_owned(), Ast::Binary(_, _, _)));
                assert!(matches!(*rhs.to_owned(), Ast::Literal(_)));
                if let Ast::Binary(lhs, _, _) = &**lhs {
                    assert!(matches!(*lhs.to_owned(), Ast::Literal(_)));
                }
            }
        }
    }

    #[test]
    fn record_literal_empty() {
        let result = parse_str_one("{}", None);
        assert!(result.is_ok());
        let result = result.unwrap();
        assert!(result.expressions.len() == 1);
        assert!(matches!(result.expressions[0], Ast::Record(_)));
        if let Ast::Record(fields) = &result.expressions[0] {
            assert_eq!(fields.len(), 0);
        }
    }

    #[test]
    fn record_literal_one() {
        let result = parse_str_one("{ x: 1 }", None);
        assert!(result.is_ok());
        let result = result.unwrap();
        assert!(result.expressions.len() == 1);
        assert!(matches!(result.expressions[0], Ast::Record(_)));
        if let Ast::Record(fields) = &result.expressions[0] {
            assert_eq!(fields.len(), 1);
            let fields = fields.iter().collect::<Vec<_>>();
            assert!(matches!(fields[0].0, RecordKey::String(_)));
            if let RecordKey::String(key) = &fields[0].0 {
                assert_eq!(key, "x");
            }
            assert!(matches!(fields[0].1, Ast::Literal(_)));
            assert_eq!(fields[0].1, Ast::Literal(make_u1(1)));
        }
    }

    #[test]
    fn record_literal_two() {
        let result = parse_str_one("{ x: 1, y: 2 }", None);
        assert!(result.is_ok());
        let result = result.unwrap();
        assert!(result.expressions.len() == 1);
        assert!(matches!(result.expressions[0], Ast::Record(_)));
        if let Ast::Record(fields) = &result.expressions[0] {
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
            assert!(matches!(fields[0].1, Ast::Literal(_)));
            assert!(matches!(fields[1].1, Ast::Literal(_)));
            assert_eq!(fields[0].1, Ast::Literal(make_u1(1)));
            assert_eq!(fields[1].1, Ast::Literal(make_u8(2)));
        }
    }

    #[test]
    fn record_literal_nested() {
        let result = parse_str_one("{ x: { y: 1 } }", None);
        assert!(result.is_ok());
        let result = result.unwrap();
        assert!(result.expressions.len() == 1);
        assert!(matches!(result.expressions[0], Ast::Record(_)));
        if let Ast::Record(fields) = &result.expressions[0] {
            assert_eq!(fields.len(), 1);
            let fields = fields.iter().collect::<Vec<_>>();
            assert!(matches!(fields[0].0, RecordKey::String(_)));
            if let RecordKey::String(key) = &fields[0].0 {
                assert_eq!(key, "x");
            }
            assert!(matches!(fields[0].1, Ast::Record(_)));
            if let Ast::Record(inner_fields) = &fields[0].1 {
                assert_eq!(inner_fields.len(), 1);
                let inner_fields = inner_fields.iter().collect::<Vec<_>>();
                assert!(matches!(inner_fields[0].0, RecordKey::String(_)));
                if let RecordKey::String(key) = &inner_fields[0].0 {
                    assert_eq!(key, "y");
                }
                assert!(matches!(inner_fields[0].1, Ast::Literal(_)));
                assert_eq!(inner_fields[0].1, Ast::Literal(make_u1(1)));
            }
        }
    }

    #[test]
    fn record_nested_block() {
        let result = parse_str_one("{ x: { 1 + 2 } }", None);
        assert!(result.is_ok());
        let result = result.unwrap();
        assert!(result.expressions.len() == 1);
        assert!(matches!(result.expressions[0], Ast::Record(_)));
        if let Ast::Record(fields) = &result.expressions[0] {
            assert_eq!(fields.len(), 1);
            let fields = fields.iter().collect::<Vec<_>>();
            assert!(matches!(fields[0].0, RecordKey::String(_)));
            if let RecordKey::String(key) = &fields[0].0 {
                assert_eq!(key, "x");
            }
            assert!(matches!(fields[0].1, Ast::Block(_)));
            if let Ast::Block(inner) = &fields[0].1 {
                assert_eq!(inner.len(), 1);
                assert!(matches!(inner[0], Ast::Binary(_, _, _)));
            }
        }
    }

    #[test]
    fn block_one() {
        let result = parse_str_one("{ 1 }", None);
        assert!(result.is_ok());
        let result = result.unwrap();
        assert!(result.expressions.len() == 1);
        assert!(matches!(result.expressions[0], Ast::Block(_)));
        if let Ast::Block(inner) = &result.expressions[0] {
            assert_eq!(inner.len(), 1);
            assert!(matches!(inner[0], Ast::Literal(_)));
        }
    }

    #[test]
    fn block_two() {
        let result = parse_str_one("{ 1; 2 }", None);
        assert!(result.is_ok());
        let result = result.unwrap();
        assert!(result.expressions.len() == 1);
        assert!(matches!(result.expressions[0], Ast::Block(_)));
        if let Ast::Block(inner) = &result.expressions[0] {
            assert_eq!(inner.len(), 2);
            assert!(matches!(inner[0], Ast::Literal(_)));
            assert!(matches!(inner[1], Ast::Literal(_)));
        }
    }

    #[test]
    fn block_three() {
        let result = parse_str_one("{ 1; 2; 3 }", None);
        assert!(result.is_ok());
        let result = result.unwrap();
        assert!(result.expressions.len() == 1);
        assert!(matches!(result.expressions[0], Ast::Block(_)));
        if let Ast::Block(inner) = &result.expressions[0] {
            assert_eq!(inner.len(), 3);
            assert!(matches!(inner[0], Ast::Literal(_)));
            assert!(matches!(inner[1], Ast::Literal(_)));
            assert!(matches!(inner[2], Ast::Literal(_)));
        }
    }

    #[test]
    fn block_three_no_semicolon() {
        let result = parse_str_one("{ 1 2 3 }", None);
        assert!(result.is_ok());
        let result = result.unwrap();
        assert!(result.expressions.len() == 1);
        assert!(matches!(result.expressions[0], Ast::Block(_)));
        if let Ast::Block(inner) = &result.expressions[0] {
            assert_eq!(inner.len(), 3);
            assert!(matches!(inner[0], Ast::Literal(_)));
            assert!(matches!(inner[1], Ast::Literal(_)));
            assert!(matches!(inner[2], Ast::Literal(_)));
        }
    }

    #[test]
    fn block_nested() {
        let result = parse_str_one("{ { 1 } }", None);
        assert!(result.is_ok());
        let result = result.unwrap();
        assert!(result.expressions.len() == 1);
        assert!(matches!(result.expressions[0], Ast::Block(_)));
        if let Ast::Block(inner) = &result.expressions[0] {
            assert_eq!(inner.len(), 1);
            assert!(matches!(inner[0], Ast::Block(_)));
        }
    }

    #[test]
    fn block_nested_two() {
        let result = parse_str_one("{ { 1; 2 } }", None);
        assert!(result.is_ok());
        let result = result.unwrap();
        assert!(result.expressions.len() == 1);
        assert!(matches!(result.expressions[0], Ast::Block(_)));
        if let Ast::Block(inner) = &result.expressions[0] {
            assert_eq!(inner.len(), 1);
            assert!(matches!(inner[0], Ast::Block(_)));
            if let Ast::Block(inner_inner) = &inner[0] {
                assert_eq!(inner_inner.len(), 2);
                assert!(matches!(inner_inner[0], Ast::Literal(_)));
                assert!(matches!(inner_inner[1], Ast::Literal(_)));
            }
        }
    }
}
