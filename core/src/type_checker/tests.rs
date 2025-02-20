#[cfg(test)]
mod tests {
    use std::vec;

    use crate::{
        interpreter::value::Value,
        parser::parser::from_string,
        stdlib::init::stdlib,
        type_checker::{
            checked_ast::CheckedAst,
            checker::TypeChecker,
            types::{std_types, Type, TypeTrait},
        },
    };

    #[test]
    fn types() {
        let types = [
            "unit", "str", "char", "bool", "u1", "u8", "u16", "u32", "u64", "u128", "ubig", "i8",
            "i16", "i32", "i64", "i128", "ibig", "f32", "f64", "fbig",
        ];
        let mut parser = from_string(types.join(" "));
        stdlib().init_parser(&mut parser);
        let mut checker = TypeChecker::default();
        stdlib().init_type_checker(&mut checker);
        let ast = parser.parse_all().unwrap();
        let checked_ast = checker.check_top_exprs(&ast).unwrap();
        assert!(checked_ast.iter().zip(types).all(|(ast, ty)| {
            if let CheckedAst::Literal(Value::Type(t), _) = ast {
                t.to_string() == ty
            } else {
                false
            }
        }))
    }

    #[test]
    fn subtype_sum() {
        let sum = Type::Sum(vec![std_types::BOOL, std_types::UNIT]);
        assert!(std_types::BOOL.subtype(&sum));
    }

    #[test]
    fn subtype_sum_sum() {
        let inner = Type::Sum(vec![std_types::BOOL, std_types::UNIT]);
        let outer = Type::Sum(vec![inner.clone(), std_types::CHAR]);
        assert!(inner.subtype(&outer));
        assert!(!outer.subtype(&inner));
        assert!(std_types::CHAR.subtype(&outer));
        assert!(std_types::BOOL.subtype(&outer));
    }
}
