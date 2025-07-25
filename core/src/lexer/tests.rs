#[cfg(test)]
mod tests {

    use std::io::Read;

    use crate::{
        interpreter::number::{FloatingPoint, Number, UnsignedInteger},
        lexer::{
            lexer::{from_str, Lexer},
            token::TokenKind,
        },
        parser::op::intrinsic_operators,
        stdlib::init::stdlib,
    };

    fn init<R: Read>(lexer: &mut Lexer<R>) {
        intrinsic_operators()
            .iter()
            .for_each(|op| lexer.add_operator(op.symbol.clone()));
        stdlib().init_lexer(lexer);
    }

    #[test]
    fn function() {
        let mut lexer = from_str("add a b = a + b");
        init(&mut lexer);

        let token = TokenKind::Identifier("add".to_string());
        assert_eq!(lexer.next_token().unwrap().token, token);

        let token = TokenKind::Identifier("a".to_string());
        assert_eq!(lexer.next_token().unwrap().token, token);

        let token = TokenKind::Identifier("b".to_string());
        assert_eq!(lexer.next_token().unwrap().token, token);

        let token = TokenKind::Op("=".to_string());
        assert_eq!(lexer.next_token().unwrap().token, token);

        let token = TokenKind::Identifier("a".to_string());
        assert_eq!(lexer.next_token().unwrap().token, token);

        let token = TokenKind::Op("+".to_string());
        assert_eq!(lexer.next_token().unwrap().token, token);

        let token = TokenKind::Identifier("b".to_string());
        assert_eq!(lexer.next_token().unwrap().token, token);

        let token = TokenKind::EndOfFile;
        assert_eq!(lexer.next_token().unwrap().token, token);
    }

    #[test]
    fn assign() {
        let mut lexer = from_str("x = 1 + 2;");
        init(&mut lexer);

        let token = TokenKind::Identifier("x".to_string());
        assert_eq!(lexer.next_token().unwrap().token, token);

        let token = TokenKind::Op("=".to_string());
        assert_eq!(lexer.next_token().unwrap().token, token);

        let token = TokenKind::Number(Number::UnsignedInteger(UnsignedInteger::UInt1(1)));
        assert_eq!(lexer.next_token().unwrap().token, token);

        let token = TokenKind::Op("+".to_string());
        assert_eq!(lexer.next_token().unwrap().token, token);

        let token = TokenKind::Number(Number::UnsignedInteger(UnsignedInteger::UInt8(2)));
        assert_eq!(lexer.next_token().unwrap().token, token);

        let token = TokenKind::SemiColon;
        assert_eq!(lexer.next_token().unwrap().token, token);

        let token = TokenKind::EndOfFile;
        assert_eq!(lexer.next_token().unwrap().token, token);
    }

    #[test]
    fn many_match_operators() {
        let mut lexer = from_str("== = === ====");
        let equals = "==".to_string();
        let assignment = "=".to_string();
        let strict_equals = "===".to_string();
        lexer.add_operator(equals.clone());
        lexer.add_operator(strict_equals.clone());
        lexer.add_operator(assignment.clone());
        // ==

        let token = TokenKind::Op(equals);
        assert_eq!(lexer.next_token().unwrap().token, token);
        // =

        let token = TokenKind::Op(assignment.clone());
        assert_eq!(lexer.next_token().unwrap().token, token);
        // ===

        let token = TokenKind::Op(strict_equals.clone());
        assert_eq!(lexer.next_token().unwrap().token, token);
        // ====

        let token = TokenKind::Op(strict_equals);
        assert_eq!(lexer.next_token().unwrap().token, token);

        let token = TokenKind::Op(assignment);
        assert_eq!(lexer.next_token().unwrap().token, token);

        let token = TokenKind::EndOfFile;
        assert_eq!(lexer.next_token().unwrap().token, token);
    }

    #[test]
    fn string() {
        let mut lexer = from_str(r#""Hello, World!""#);
        init(&mut lexer);

        let token = TokenKind::String("Hello, World!".to_string());
        assert_eq!(lexer.next_token().unwrap().token, token);

        let token = TokenKind::EndOfFile;
        assert_eq!(lexer.next_token().unwrap().token, token);
    }

    #[test]
    fn string_escape() {
        let mut lexer = from_str(r#""Hello, \"World\"!""#);
        init(&mut lexer);

        let token = TokenKind::String("Hello, \\\"World\\\"!".to_string());
        assert_eq!(lexer.next_token().unwrap().token, token);

        let token = TokenKind::EndOfFile;
        assert_eq!(lexer.next_token().unwrap().token, token);
    }

    #[test]
    fn char() {
        let mut lexer = from_str(r#"'a'"#);
        init(&mut lexer);

        let token = TokenKind::Char('a');
        assert_eq!(lexer.next_token().unwrap().token, token);

        let token = TokenKind::EndOfFile;
        assert_eq!(lexer.next_token().unwrap().token, token);
    }

    #[test]
    fn char_escape() {
        let mut lexer = from_str(r#"'\\'"#);
        init(&mut lexer);

        let token = TokenKind::Char('\\');
        assert_eq!(lexer.next_token().unwrap().token, token);

        let token = TokenKind::EndOfFile;
        assert_eq!(lexer.next_token().unwrap().token, token);
    }

    #[test]
    fn float() {
        let mut lexer = from_str("123.456");

        let token = TokenKind::Number(Number::FloatingPoint(FloatingPoint::Float32(123.456)));
        assert_eq!(lexer.next_token().unwrap().token, token);

        let token = TokenKind::EndOfFile;
        assert_eq!(lexer.next_token().unwrap().token, token);
    }

    #[test]
    fn integer() {
        let mut lexer = from_str("123");

        let token = TokenKind::Number(Number::UnsignedInteger(UnsignedInteger::UInt8(123)));
        assert_eq!(lexer.next_token().unwrap().token, token);

        let token = TokenKind::EndOfFile;
        assert_eq!(lexer.next_token().unwrap().token, token);
    }

    #[test]
    fn identifier() {
        let mut lexer = from_str("abc_123");
        stdlib().init_lexer(&mut lexer);

        let token = TokenKind::Identifier("abc_123".to_string());
        assert_eq!(lexer.next_token().unwrap().token, token);

        let token = TokenKind::EndOfFile;
        assert_eq!(lexer.next_token().unwrap().token, token);
    }

    #[test]
    fn keywords() {
        let mut lexer = from_str("true false");
        init(&mut lexer);

        let token = TokenKind::Boolean(true);
        assert_eq!(lexer.next_token().unwrap().token, token);

        let token = TokenKind::Boolean(false);
        assert_eq!(lexer.next_token().unwrap().token, token);

        let token = TokenKind::EndOfFile;
        assert_eq!(lexer.next_token().unwrap().token, token);
    }

    #[test]
    fn comment() {
        let mut lexer = from_str("// This is a comment");

        let token = TokenKind::Comment(" This is a comment".to_string());
        assert_eq!(lexer.next_token().unwrap().token, token);

        let token = TokenKind::EndOfFile;
        assert_eq!(lexer.next_token().unwrap().token, token);
    }

    #[test]
    fn commas() {
        let mut lexer = from_str("a, b, c");
        init(&mut lexer);

        let token = TokenKind::Identifier("a".to_string());
        assert_eq!(lexer.next_token().unwrap().token, token);

        let token = TokenKind::Op(",".to_string());
        assert_eq!(lexer.next_token().unwrap().token, token);

        let token = TokenKind::Identifier("b".to_string());
        assert_eq!(lexer.next_token().unwrap().token, token);

        let token = TokenKind::Op(",".to_string());
        assert_eq!(lexer.next_token().unwrap().token, token);

        let token = TokenKind::Identifier("c".to_string());
        assert_eq!(lexer.next_token().unwrap().token, token);

        let token = TokenKind::EndOfFile;
        assert_eq!(lexer.next_token().unwrap().token, token);
    }

    #[test]
    fn colon() {
        let mut lexer = from_str("a: b");

        let token = TokenKind::Identifier("a".to_string());
        assert_eq!(lexer.next_token().unwrap().token, token);

        let token = TokenKind::Colon;
        assert_eq!(lexer.next_token().unwrap().token, token);

        let token = TokenKind::Identifier("b".to_string());
        assert_eq!(lexer.next_token().unwrap().token, token);

        let token = TokenKind::EndOfFile;
        assert_eq!(lexer.next_token().unwrap().token, token);
    }

    #[test]
    fn parens() {
        let mut lexer = from_str("(a)");

        let token = TokenKind::LeftParen {
            is_function_call: false,
        };
        assert_eq!(lexer.next_token().unwrap().token, token);

        let token = TokenKind::Identifier("a".to_string());
        assert_eq!(lexer.next_token().unwrap().token, token);

        let token = TokenKind::RightParen;
        assert_eq!(lexer.next_token().unwrap().token, token);

        let token = TokenKind::EndOfFile;
        assert_eq!(lexer.next_token().unwrap().token, token);
    }

    #[test]
    fn braces() {
        let mut lexer = from_str("{a}");

        let token = TokenKind::LeftBrace;
        assert_eq!(lexer.next_token().unwrap().token, token);

        let token = TokenKind::Identifier("a".to_string());
        assert_eq!(lexer.next_token().unwrap().token, token);

        let token = TokenKind::RightBrace;
        assert_eq!(lexer.next_token().unwrap().token, token);

        let token = TokenKind::EndOfFile;
        assert_eq!(lexer.next_token().unwrap().token, token);
    }

    #[test]
    fn brackets() {
        let mut lexer = from_str("[a]");

        let token = TokenKind::LeftBracket;
        assert_eq!(lexer.next_token().unwrap().token, token);

        let token = TokenKind::Identifier("a".to_string());
        assert_eq!(lexer.next_token().unwrap().token, token);

        let token = TokenKind::RightBracket;
        assert_eq!(lexer.next_token().unwrap().token, token);

        let token = TokenKind::EndOfFile;
        assert_eq!(lexer.next_token().unwrap().token, token);
    }

    #[test]
    fn operators() {
        let mut lexer = from_str("a + b");
        lexer.add_operator("+".to_string());

        let token = TokenKind::Identifier("a".to_string());
        assert_eq!(lexer.next_token().unwrap().token, token);

        let token = TokenKind::Op("+".to_string());
        assert_eq!(lexer.next_token().unwrap().token, token);

        let token = TokenKind::Identifier("b".to_string());
        assert_eq!(lexer.next_token().unwrap().token, token);

        let token = TokenKind::EndOfFile;
        assert_eq!(lexer.next_token().unwrap().token, token);
    }

    #[test]
    fn semicolon() {
        let mut lexer = from_str("a; b");

        let token = TokenKind::Identifier("a".to_string());
        assert_eq!(lexer.next_token().unwrap().token, token);

        let token = TokenKind::SemiColon;
        assert_eq!(lexer.next_token().unwrap().token, token);

        let token = TokenKind::Identifier("b".to_string());
        assert_eq!(lexer.next_token().unwrap().token, token);

        let token = TokenKind::EndOfFile;
        assert_eq!(lexer.next_token().unwrap().token, token);
    }
}
