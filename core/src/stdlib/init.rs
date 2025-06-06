use std::{collections::HashMap, io::Read};

use colorful::Colorful;

use crate::{
    interpreter::{
        env::Environment,
        number::{FloatingPoint, Number},
        value::{Function, Value},
    },
    lexer::lexer::Lexer,
    parser::{
        op::{prec, OpAssoc, OpHandler, OpPos, OpSignature, Operator, RuntimeOpHandler},
        parser::Parser,
    },
    stdlib::arithmetic,
    type_checker::{
        checker::TypeChecker,
        types::{std_types, GetType, Type, TypeTrait},
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
    constants: Vec<(Str, Value)>,
    functions: Vec<(&'static str, Function)>,
}

impl Initializer {
    pub fn init_lexer(&self, lexer: &mut Lexer<impl Read>) {
        log::trace!("Initializing lexer with {} operators", self.operators.len());
        for op in &self.operators {
            // TODO: Why does this only add static operators to the lexer?
            if let OpHandler::Static(_) = &op.handler {
                lexer.add_operator(op.info.symbol.clone());
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
            if let Err(e) = parser.define_op(op.info.clone()) {
                panic!(
                    "Parser initialization failed when adding operator '{:?}': {:?}",
                    op, e
                );
            }
        }
        for ty in self.types.values() {
            match ty {
                Type::Literal(ref name) => parser.add_type(name.to_string()),
                Type::Constructor(ref name, _, _) => parser.add_type(name.to_string()),
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
        for (name, val) in &self.constants {
            type_checker.add_variable(name.to_string(), val.get_type().clone());
        }
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
            self.constants.len(),
            self.functions.len(),
            self.operators.len(),
            self.types.len()
        );
        for (name, val) in &self.constants {
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
                OpHandler::Runtime(RuntimeOpHandler {
                    function_name,
                    signature,
                }) => {
                    // TODO: Implement support for function overloading (multiple variations)
                    // Assert that the function is already in the environment
                    if let Some(func) = env.lookup_function(function_name) {
                        if !signature.function_type().equals(func.get_fn_type()).success {
                            panic!(
                                "Function type mismatch for operator {}: expected {}, but got {}",
                                op.info.symbol.clone().yellow(),
                                signature.function_type().pretty_print(),
                                func.get_type().pretty_print()
                            );
                        }
                    }
                }
                // Skip static operators
                OpHandler::Static(_) => {}
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

//--------------------------------------------------------------------------------------//
//                                   Standard Library                                   //
//--------------------------------------------------------------------------------------//

pub fn stdlib() -> Initializer {
    Initializer {
        operators: vec![
            Operator::new_runtime(
                "add".into(),
                "+".into(),
                OpPos::Infix,
                prec::ADDITIVE_PREC,
                OpAssoc::Left,
                false,
                OpSignature::from_function(arithmetic::add().get_fn_type()),
            ),
            Operator::new_runtime(
                "sub".into(),
                "-".into(),
                OpPos::Infix,
                prec::ADDITIVE_PREC,
                OpAssoc::Left,
                false,
                OpSignature::from_function(arithmetic::sub().get_fn_type()),
            ),
            Operator::new_runtime(
                "mul".into(),
                "*".into(),
                OpPos::Infix,
                prec::MULTIPLICATIVE_PREC,
                OpAssoc::Left,
                false,
                OpSignature::from_function(arithmetic::mul().get_fn_type()),
            ),
            Operator::new_runtime(
                "div".into(),
                "/".into(),
                OpPos::Infix,
                prec::MULTIPLICATIVE_PREC,
                OpAssoc::Left,
                false,
                OpSignature::from_function(arithmetic::div().get_fn_type()),
            ),
            Operator::new_runtime(
                "eq".into(),
                "==".into(),
                OpPos::Infix,
                prec::EQUALITY_PREC,
                OpAssoc::Left,
                false,
                OpSignature::from_function(logical::eq().get_fn_type()),
            ),
        ],
        types: vec![
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
            ("List".into(), std_types::LIST()),
            ("Map".into(), std_types::MAP()),
        ])
        .collect(),
        constants: vec![
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
