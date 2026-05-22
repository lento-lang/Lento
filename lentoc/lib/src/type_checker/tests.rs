#[cfg(test)]
mod tests {
    use std::vec;

    use crate::{
        interpreter::value::Value,
        parser::{parser::from_string, pattern::BindPattern},
        stdlib::init::{stdlib, Initializer},
        type_checker::{
            checked_ast::CheckedAst,
            checker::{TypeChecker, TypeCheckerResult, TypeErrorVariant},
            types::{std_types, Type, TypeTrait},
        },
    };

    fn check_str_one(input: &str, init: Option<&Initializer>) -> TypeCheckerResult<CheckedAst> {
        let mut parser = from_string(input.to_string());
        let mut checker = TypeChecker::default();
        if let Some(init) = init {
            init.init_parser(&mut parser);
            init.init_type_checker(&mut checker);
        }
        match parser.parse_one() {
            Ok(ast) => checker.check_expr(&ast),
            Err(err) => Err(TypeErrorVariant::ParseError(err)),
        }
    }

    #[test]
    fn types() {
        let types = [
            "unit", "str", "char", "bool", "u1", "u8", "u16", "u32", "u64", "u128", "ubig", "i8",
            "i16", "i32", "i64", "i128", "ibig", "f32", "f64", "fbig",
        ];
        let mut parser = from_string(types.join("; "));
        stdlib().init_parser(&mut parser);
        let mut checker = TypeChecker::default();
        stdlib().init_type_checker(&mut checker);
        let ast = parser.parse_all().unwrap();
        let checked_ast = checker.check_top_exprs(&ast).unwrap();
        assert!(checked_ast.iter().zip(types).all(|(ast, ty)| {
            if let CheckedAst::LiteralValue {
                value: Value::Type(t),
                info: _,
            } = ast
            {
                t.to_string() == ty
            } else {
                false
            }
        }))
    }

    #[test]
    fn subtype_sum() {
        let sum = Type::Sum(vec![std_types::BOOL, std_types::UNIT]);
        assert!(std_types::BOOL.subtype(&sum).success);
    }

    #[test]
    fn subtype_sum_sum() {
        let inner = Type::Sum(vec![std_types::BOOL, std_types::UNIT]);
        let outer = Type::Sum(vec![inner.clone(), std_types::CHAR]);
        assert!(inner.subtype(&outer).success);
        assert!(!outer.subtype(&inner).success);
        assert!(std_types::CHAR.subtype(&outer).success);
        assert!(std_types::BOOL.subtype(&outer).success);
    }

    #[test]
    fn invalid_function() {
        let result = check_str_one("() 1", Some(&stdlib()));
        dbg!("{:?}", &result);
        assert!(result.is_err());
    }

    #[test]
    fn function_def_with_return_type_single_no_parens_block() {
        let result = check_str_one("fn f(x) -> int { x + 5 }", Some(&stdlib())).unwrap();
        if let CheckedAst::FunctionDef { name, params, .. } = result {
            assert_eq!(name, "f");
            assert_eq!(params.len(), 1);
            if let BindPattern::Variable { name, .. } = &params[0].pattern {
                assert_eq!(name, "x");
            }
        } else {
            panic!(
                "Expected function definition with return type and no parens: {:?}",
                result
            );
        }
    }

    #[test]
    fn function_def_with_return_type_many_no_parens_block() {
        let result = check_str_one("fn add(x, y) -> int { x + y }", Some(&stdlib())).unwrap();
        if let CheckedAst::FunctionDef { name, params, .. } = result {
            assert_eq!(name, "add");
            assert_eq!(params.len(), 2);
            assert!(matches!(params[0].pattern, BindPattern::Variable { .. }));
            assert!(matches!(params[1].pattern, BindPattern::Variable { .. }));
            if let BindPattern::Variable { name, .. } = &params[0].pattern {
                assert_eq!(name, "x");
            }
            if let BindPattern::Variable { name, .. } = &params[1].pattern {
                assert_eq!(name, "y");
            }
        } else {
            panic!(
                "Expected function definition with return type and no parens: {:?}",
                result
            );
        }
    }

    #[test]
    fn checked_type_decl() {
        let result = check_str_one("type Foo = u8", Some(&stdlib())).unwrap();
        assert!(matches!(result, CheckedAst::TypeDecl { .. }));
    }

    #[test]
    fn checked_function_def_has_all_params() {
        let result = check_str_one("fn add(x, y) = 1", Some(&stdlib())).unwrap();
        if let CheckedAst::FunctionDef { params, .. } = result {
            assert_eq!(params.len(), 2);
        } else {
            panic!("Expected CheckedAst::FunctionDef");
        }
    }

    #[test]
    fn checked_let_keyword_decl() {
        let result = check_str_one("let x = 1", Some(&stdlib())).unwrap();
        assert!(matches!(result, CheckedAst::Let { .. }));
    }
}
