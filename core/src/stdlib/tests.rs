#[cfg(test)]
mod tests {
    use crate::{interpreter::env::Environment, parser, stdlib::init::stdlib, util::str::Str};

    #[test]
    fn test_init_parser() {
        let mut parser = parser::parser::from_str(""); // Initialize the parser (required for the lexer to work)
        stdlib().init_parser(&mut parser);
        assert!(parser.get_op("=").is_some());
        assert!(parser.get_op("+").is_some());
        assert!(parser.get_op(",").is_some());
    }

    #[test]
    fn test_init_environment() {
        let mut env = Environment::new(Str::empty());
        stdlib().init_environment(&mut env);
        // Variables
        assert!(env.lookup_variable("pi").is_some());
        assert!(env.lookup_variable("tau").is_some());
        assert!(env.lookup_variable("e").is_some());
        assert!(env.lookup_variable("phi").is_some());
        assert!(env.lookup_variable("sqrt2").is_some());
        assert!(env.lookup_variable("ln2").is_some());
        assert!(env.lookup_variable("ln10").is_some());
        assert!(env.lookup_variable("inf").is_some());
        assert!(env.lookup_variable("NaN").is_some());
        // Functions
        assert!(env.lookup_function("print").is_some());
        assert!(env.lookup_function("typeof").is_some());
        assert!(env.lookup_function("exit").is_some());
        assert!(env.lookup_function("add").is_some());
        assert!(env.lookup_function("sub").is_some());
        assert!(env.lookup_function("mul").is_some());
        assert!(env.lookup_function("div").is_some());
        assert!(env.lookup_function("eq").is_some());
        // Types
        assert!(env.get_type("any").is_some());
        assert!(env.get_type("unit").is_some());
        assert!(env.get_type("str").is_some());
        assert!(env.get_type("char").is_some());
        assert!(env.get_type("bool").is_some());
        assert!(env.get_type("u1").is_some());
        assert!(env.get_type("u8").is_some());
        assert!(env.get_type("u16").is_some());
        assert!(env.get_type("u32").is_some());
        assert!(env.get_type("u64").is_some());
        assert!(env.get_type("u128").is_some());
        assert!(env.get_type("ubig").is_some());
        assert!(env.get_type("i8").is_some());
        assert!(env.get_type("i16").is_some());
        assert!(env.get_type("i32").is_some());
        assert!(env.get_type("i64").is_some());
        assert!(env.get_type("i128").is_some());
        assert!(env.get_type("ibig").is_some());
        assert!(env.get_type("f32").is_some());
        assert!(env.get_type("f64").is_some());
        assert!(env.get_type("fbig").is_some());
        assert!(env.get_type("uint").is_some());
        assert!(env.get_type("int").is_some());
        assert!(env.get_type("float").is_some());
        assert!(env.get_type("num").is_some());
    }
}
