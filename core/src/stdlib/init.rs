use std::{collections::HashMap, io::Read};

use colorful::Colorful;

use crate::{
    interpreter::{
        self,
        env::Environment,
        number::{FloatingPoint, Number},
        value::{Function, RecordKey, Value},
    },
    lexer::lexer::Lexer,
    parser::{
        ast::Ast,
        error::ParseError,
        op::{
            default_operator_precedence, Operator, OperatorAssociativity, OperatorHandler,
            OperatorPosition, OperatorSignature, RuntimeOperatorHandler, StaticOperatorAst,
        },
        parser::Parser,
    },
    stdlib::arithmetic,
    type_checker::{
        checked_ast::CheckedParam,
        checker::TypeChecker,
        types::{std_types, Type, TypeTrait},
    },
    util::{
        error::{BaseErrorExt, LineInfo},
        str::Str,
    },
};

use super::{logical, system};

pub struct Initializer {
    operators: Vec<Operator>,
    types: HashMap<String, Type>,
    values: Vec<(Str, Value)>,
    functions: Vec<(&'static str, Function)>,
}

impl Initializer {
    pub fn init_lexer(&self, lexer: &mut Lexer<impl Read>) {
        log::trace!("Initializing lexer with {} operators", self.operators.len());
        for op in &self.operators {
            // TODO: Why does this only add static operators to the lexer?
            if let OperatorHandler::Static(_) = &op.handler {
                lexer.operators.insert(op.info.symbol.clone());
            }
        }
    }

    pub fn init_parser(&self, parser: &mut Parser<impl Read>) {
        log::trace!(
            "Initializing parser with {} operators and {} types",
            self.operators.len(),
            self.types.len()
        );
        for op in &self.operators {
            if let Err(e) = parser.define_op(op.clone()) {
                panic!(
                    "Parser initialization failed when adding operator '{:?}': {:?}",
                    op, e
                );
            }
        }
        for ty in self.types.values() {
            match ty {
                Type::Literal(ref name) => parser.add_type(name.to_string()),
                Type::Alias(ref name, _) => parser.add_type(name.to_string()),
                _ => panic!("Expected literal or alias type but got {:?}", ty),
            }
        }
    }

    pub fn init_type_checker(&self, type_checker: &mut TypeChecker) {
        log::trace!(
            "Initializing type checker with {} types, {} functions, and {} operators",
            self.types.len(),
            self.functions.len(),
            self.operators.len()
        );
        for op in &self.operators {
            type_checker.add_operator(op.clone());
        }
        for (name, ty) in &self.types {
            type_checker.add_type(name, ty.clone());
        }
        for (name, func) in &self.functions {
            // TODO: Implement support for function overloading (multiple variations)
            type_checker.add_function(name, func.get_fn_type().clone());
        }
    }

    pub fn init_environment(&self, env: &mut Environment) {
        log::trace!(
            "Initializing environment with {} values, {} functions, {} operators and {} types",
            self.values.len(),
            self.functions.len(),
            self.operators.len(),
            self.types.len()
        );
        for (name, val) in &self.values {
            if let Err(e) = env.add_value(name.clone(), val.clone(), &LineInfo::default()) {
                panic!(
                    "Environment initialization failed when adding value {}: {}",
                    name.to_string().yellow(),
                    e.message()
                );
            }
        }
        for (name, func) in &self.functions {
            if let Err(e) = env.add_value(
                Str::String(name.to_string()),
                Value::Function(Box::new(func.clone())),
                &LineInfo::default(),
            ) {
                panic!(
                    "Environment initialization failed when adding function {}: {}",
                    name.to_string().yellow(),
                    e.message()
                );
            }
        }
        for op in &self.operators {
            match &op.handler {
                OperatorHandler::Runtime(RuntimeOperatorHandler {
                    function_name,
                    signature,
                }) => {
                    // TODO: Implement support for function overloading (multiple variations)
                    // Assert that the function is already in the environment
                    if let Some(func) = env.lookup_function(function_name) {
                        if !signature.function_type().equals(func.get_fn_type()).success {
                            panic!(
                                "Function type mismatch for operator {}: expected {}, but got {}",
                                op.info.name.clone().yellow(),
                                signature.function_type().pretty_print(),
                                func.get_type().pretty_print()
                            );
                        }
                    }
                }
                // Skip static operators
                OperatorHandler::Parse(_) => {}
                OperatorHandler::Static(_) => {}
            }
        }
        for (name, ty) in &self.types {
            if let Err(e) = env.add_type(Str::String(name.to_string()), ty.clone()) {
                panic!(
                    "Environment initialization failed when adding type {}: {}",
                    name.to_string().yellow(),
                    e.message()
                );
            }
        }
    }
}

pub fn stdlib() -> Initializer {
    Initializer {
        //--------------------------------------------------------------------------------------//
        //                                       Operators                                      //
        //--------------------------------------------------------------------------------------//
        operators: vec![
            // Assignment operator, native to the language
            // TODO: Implement this operator statically in the parser instead of using an operator handler
            Operator::new_parse(
                "assign".into(),
                "=".into(),
                OperatorPosition::Infix,
                default_operator_precedence::ASSIGNMENT,
                OperatorAssociativity::Right,
                false,
                |op| {
                    if let StaticOperatorAst::Infix(lhs, rhs) = op {
                        let info = lhs.info().join(rhs.info());
                        Ast::Assignment {
                            annotation: None,
                            target: Box::new(lhs),
                            expr: Box::new(rhs),
                            info,
                        }
                    } else {
                        panic!("assign expects an infix operator");
                    }
                },
            ),
            // Field access operator
            Operator::new_static(
                "field_access".into(),
                ".".into(),
                OperatorPosition::Infix,
                default_operator_precedence::MEMBER_ACCESS,
                OperatorAssociativity::Left,
                false,
                OperatorSignature::new(
                    vec![
                        CheckedParam::from_str("record", std_types::ANY),
                        CheckedParam::from_str("field", std_types::ANY),
                    ],
                    std_types::ANY.clone(),
                ),
                |op| {
                    if let StaticOperatorAst::Infix(lhs, rhs) = op {
                        let key = if let Ast::Identifier { name, .. } = &rhs {
                            RecordKey::String(name.to_string())
                        } else if let Ast::Literal {
                            value: Value::Number(interpreter::number::Number::UnsignedInteger(n)),
                            ..
                        } = &rhs
                        {
                            RecordKey::Number(Number::UnsignedInteger(n.clone()))
                        } else {
                            return Err(ParseError::new(
                                format!(
                                    "Field access via {} requires a identifier or {} literal",
                                    ".".yellow(),
                                    std_types::UINT().pretty_print_color()
                                ),
                                rhs.info().clone(),
                            )
                            .with_label(
                                format!(
                                    "This is not an identifier or {}",
                                    std_types::UINT().pretty_print_color()
                                ),
                                rhs.info().clone(),
                            )
                            .with_hint(format!(
                                "Did you mean to use indexing via {} instead?",
                                "[]".yellow()
                            )));
                        };
                        let info = lhs.info().join(rhs.info());
                        Ok(Ast::FieldAccess {
                            expr: Box::new(lhs),
                            field: key,
                            info,
                        })
                    } else {
                        panic!("field_access expects an infix operator");
                    }
                },
            ),
            // Addition operator
            Operator::new_runtime(
                "add".into(),
                "+".into(),
                OperatorPosition::Infix,
                default_operator_precedence::ADDITIVE,
                OperatorAssociativity::Left,
                false,
                OperatorSignature::from_function(arithmetic::add().get_fn_type()),
            ),
            Operator::new_runtime(
                "sub".into(),
                "-".into(),
                OperatorPosition::Infix,
                default_operator_precedence::ADDITIVE,
                OperatorAssociativity::Left,
                false,
                OperatorSignature::from_function(arithmetic::sub().get_fn_type()),
            ),
            Operator::new_runtime(
                "mul".into(),
                "*".into(),
                OperatorPosition::Infix,
                default_operator_precedence::MULTIPLICATIVE,
                OperatorAssociativity::Left,
                false,
                OperatorSignature::from_function(arithmetic::mul().get_fn_type()),
            ),
            Operator::new_runtime(
                "div".into(),
                "/".into(),
                OperatorPosition::Infix,
                default_operator_precedence::MULTIPLICATIVE,
                OperatorAssociativity::Left,
                false,
                OperatorSignature::from_function(arithmetic::div().get_fn_type()),
            ),
            // Comparison operators
            Operator::new_runtime(
                "eq".into(),
                "==".into(),
                OperatorPosition::Infix,
                default_operator_precedence::EQUALITY,
                OperatorAssociativity::Left,
                false,
                OperatorSignature::from_function(logical::eq().get_fn_type()),
            ),
        ],

        //--------------------------------------------------------------------------------------//
        //						            Built-in Types                                      //
        //--------------------------------------------------------------------------------------//
        types: vec![
            std_types::ANY,
            std_types::TYPE,
            std_types::UNIT,
            std_types::STRING,
            std_types::CHAR,
            std_types::BOOL,
            std_types::UINT1,
            std_types::UINT8,
            std_types::UINT16,
            std_types::UINT32,
            std_types::UINT64,
            std_types::UINT128,
            std_types::UINTBIG,
            std_types::INT8,
            std_types::INT16,
            std_types::INT32,
            std_types::INT64,
            std_types::INT128,
            std_types::INTBIG,
            std_types::FLOAT32,
            std_types::FLOAT64,
            std_types::FLOATBIG,
        ]
        .into_iter()
        .map(|ty| {
            if let Type::Literal(ref name) = ty {
                (name.to_string(), ty)
            } else {
                panic!("Expected literal type");
            }
        })
        .chain([
            ("uint".into(), std_types::UINT()),
            ("int".into(), std_types::INT()),
            ("float".into(), std_types::FLOAT()),
            ("num".into(), std_types::NUM()),
        ])
        .collect(),

        //--------------------------------------------------------------------------------------//
        //                                      Constants                                       //
        //--------------------------------------------------------------------------------------//
        values: vec![
            (
                Str::String("pi".to_string()),
                Value::Number(Number::FloatingPoint(FloatingPoint::Float64(
                    std::f64::consts::PI,
                ))),
            ),
            (
                Str::String("tau".to_string()),
                Value::Number(Number::FloatingPoint(FloatingPoint::Float64(
                    std::f64::consts::TAU,
                ))),
            ),
            (
                Str::String("e".to_string()),
                Value::Number(Number::FloatingPoint(FloatingPoint::Float64(
                    std::f64::consts::E,
                ))),
            ),
            (
                Str::String("phi".to_string()),
                Value::Number(Number::FloatingPoint(FloatingPoint::Float64(
                    1.618_033_988_749_895,
                ))),
            ),
            (
                Str::String("sqrt2".to_string()),
                Value::Number(Number::FloatingPoint(FloatingPoint::Float64(
                    std::f64::consts::SQRT_2,
                ))),
            ),
            (
                Str::String("ln2".to_string()),
                Value::Number(Number::FloatingPoint(FloatingPoint::Float64(
                    std::f64::consts::LN_2,
                ))),
            ),
            (
                Str::String("ln10".to_string()),
                Value::Number(Number::FloatingPoint(FloatingPoint::Float64(
                    std::f64::consts::LN_10,
                ))),
            ),
            (
                Str::String("inf".to_string()),
                Value::Number(Number::FloatingPoint(FloatingPoint::Float64(f64::INFINITY))),
            ),
            (
                Str::String("NaN".to_string()),
                Value::Number(Number::FloatingPoint(FloatingPoint::Float64(f64::NAN))),
            ),
        ],

        //--------------------------------------------------------------------------------------//
        //                                       Functions                                      //
        //--------------------------------------------------------------------------------------//
        functions: vec![
            ("add", arithmetic::add()),
            ("sub", arithmetic::sub()),
            ("mul", arithmetic::mul()),
            ("div", arithmetic::div()),
            ("eq", logical::eq()),
            ("print", system::print()),
            ("dbg", system::dbg()),
            ("typeof", system::type_of()),
            ("exit", system::exit()),
        ],
    }
}
