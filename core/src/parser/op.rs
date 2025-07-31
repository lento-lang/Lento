use crate::{
    parser::ast::Ast,
    type_checker::{
        checked_ast::CheckedParam,
        types::{FunctionType, Type},
    },
};

use super::parser::ParseResult;

//--------------------------------------------------------------------------------------//
//                               Execution Agnostic Data                                //
//--------------------------------------------------------------------------------------//

/// The position of the operator in the expression.
/// - Prefix: `-x`
/// - Infix: `x + y`
/// - Postfix: `x!`
#[derive(Clone, Debug, PartialEq, Hash, Eq)]
pub enum OpPos {
    Prefix,  // Unary operator
    Infix,   // Binary operator
    Postfix, // Unary operator
}

impl OpPos {
    pub fn is_prefix(&self) -> bool {
        matches!(self, OpPos::Prefix)
    }

    pub fn is_infix(&self) -> bool {
        matches!(self, OpPos::Infix)
    }

    pub fn is_postfix(&self) -> bool {
        matches!(self, OpPos::Postfix)
    }
}

/// Associativity of the operator.
#[derive(Clone, Debug, PartialEq, Hash, Eq)]
pub enum OpAssoc {
    Left,
    Right,
}

/// The precedence of the operator.
pub type OpPrec = u16;

/// Default precedence for operators used to define the order of operations.
/// Higher precedence operators are evaluated first.
pub mod prec {
    use super::OpPrec;

    pub const SEMICOLON_PREC: OpPrec = 200;
    pub const ASSIGNMENT_PREC: OpPrec = 300;
    pub const COMMA_PREC: OpPrec = 400;
    pub const CONDITIONAL_PREC: OpPrec = 500;
    pub const LOGICAL_OR_PREC: OpPrec = 600;
    pub const LOGICAL_AND_PREC: OpPrec = 700;
    pub const EQUALITY_PREC: OpPrec = 800;
    pub const TUPLE_PREC: OpPrec = 900;
    pub const ADDITIVE_PREC: OpPrec = 1000;
    pub const MULTIPLICATIVE_PREC: OpPrec = 1100;
    pub const EXPONENTIAL_PREC: OpPrec = 1200;
    pub const PREFIX_PREC: OpPrec = 1300;
    pub const POSTFIX_PREC: OpPrec = 1400;
    pub const MEMBER_ACCESS_PREC: OpPrec = 1500;

    /// Function application precedence.
    /// Stronger than any other default operator.
    pub const FUNCTION_APP_PREC: OpPrec = 2000;
}

//--------------------------------------------------------------------------------------//
//                                      Operators                                       //
//--------------------------------------------------------------------------------------//

#[derive(Clone, Debug)]
pub enum StaticOpAst {
    Prefix(Ast),
    Infix(Ast, Ast),
    Postfix(Ast),
}

#[derive(Clone, Debug)]
pub struct OpSignature {
    pub params: Vec<CheckedParam>,
    pub ret: Type,
}

impl OpSignature {
    pub fn new(params: Vec<CheckedParam>, ret: Type) -> Self {
        Self { params, ret }
    }

    pub fn function_type(&self) -> FunctionType {
        let mut params = self.params.iter();
        let mut func = FunctionType {
            param: params.next().unwrap().clone(),
            return_type: self.ret.clone(),
        };
        for param in params {
            func = FunctionType {
                param: param.clone(),
                return_type: Type::Function(Box::new(func)),
            };
        }
        func
    }

    pub fn from_function(function: &FunctionType) -> Self {
        let mut params = Vec::new();
        let mut func = function;
        while let Type::Function(f) = &func.return_type {
            params.push(f.param.clone());
            func = f;
        }
        params.push(func.param.clone());
        params.reverse();
        Self {
            params,
            ret: func.return_type.clone(),
        }
    }
}

//--------------------------------------------------------------------------------------//
//                                       Prelude                                        //
//--------------------------------------------------------------------------------------//

#[derive(Clone, Debug, PartialEq, Hash, Eq)]
pub struct OpInfo {
    /// The symbol of the operator
    pub symbol: String,
    /// The position of the operator
    pub position: OpPos,
    /// The precedence of the operator
    pub precedence: OpPrec,
    /// The associativity of the operator
    pub associativity: OpAssoc,
    /// If the operator allows trailing arguments
    ///
    /// ## Note
    /// **Only applicable for infix accumulate operators!**
    ///
    /// ## Example
    /// Addition operator (`+`) does usually **not** allow trailing arguments, while the comma operator (`,`) does.
    /// ```ignore
    /// a + b + c   // OK
    /// a + b + c + // Error `+` does not allow trailing arguments
    /// a, b, c     // OK
    /// a, b, c,    // OK `,` allow trailing arguments
    /// ```
    pub allow_trailing: bool,
}

#[derive(Clone, Debug)]
pub struct RuntimeOpHandler {
    pub function_name: String,
    pub signature: OpSignature,
}

#[derive(Clone, Debug)]
pub struct StaticOpHandler {
    pub signature: OpSignature,
    pub handler: fn(StaticOpAst) -> ParseResult,
}

#[derive(Clone, Debug)]
pub enum OpHandler {
    /// Runtime operators (functions)
    Runtime(RuntimeOpHandler),
    /// The compile-time handler for the operator
    /// (macros or syntax extensions/sugar)
    /// 1. The signature of the operator. This is used for type checking and inference on the operator in expressions.
    /// 2. The native handler function for the operator called at compile-time
    Static(StaticOpHandler),
}

#[derive(Clone, Debug)]
pub struct Operator {
    /// Basic information about the operator
    /// required for parsing and type checking.
    pub info: OpInfo,
    /// The handler for the operator
    pub handler: OpHandler,
}

impl Operator {
    pub fn signature(&self) -> OpSignature {
        match &self.handler {
            OpHandler::Runtime(RuntimeOpHandler { signature, .. }) => signature.clone(),
            OpHandler::Static(StaticOpHandler { signature, .. }) => signature.clone(),
        }
    }

    pub fn new_runtime(
        function_name: String,
        symbol: String,
        position: OpPos,
        precedence: OpPrec,
        associativity: OpAssoc,
        allow_trailing: bool,
        signature: OpSignature,
    ) -> Self {
        Self {
            info: OpInfo {
                symbol,
                position,
                precedence,
                associativity,
                allow_trailing,
            },
            handler: OpHandler::Runtime(RuntimeOpHandler {
                function_name,
                signature,
            }),
        }
    }

    #[allow(clippy::too_many_arguments)]
    pub fn new_static(
        symbol: String,
        position: OpPos,
        precedence: OpPrec,
        associativity: OpAssoc,
        allow_trailing: bool,
        signature: OpSignature,
        handler: fn(StaticOpAst) -> ParseResult,
    ) -> Self {
        Self {
            info: OpInfo {
                symbol,
                position,
                precedence,
                associativity,
                allow_trailing,
            },
            handler: OpHandler::Static(StaticOpHandler { signature, handler }),
        }
    }
}
