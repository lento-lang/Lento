use crate::{
    interpreter::value::{RecordKey, Value},
    lexer::token::LineInfo,
};

use super::op::OperatorInfo;

#[derive(Debug, Clone)]
pub struct ParamAst {
    pub name: String,
    pub ty: Option<TypeAst>,
    pub info: LineInfo,
}

impl PartialEq for ParamAst {
    fn eq(&self, other: &Self) -> bool {
        self.name == other.name && self.ty == other.ty
    }
}

#[derive(Debug, Clone)]
pub enum TypeAst {
    Identifier(String, LineInfo),
}

impl TypeAst {
    pub fn print_sexpr(&self) -> String {
        match self {
            TypeAst::Identifier(name, _) => name.clone(),
        }
    }
}

impl PartialEq for TypeAst {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Identifier(l0, _), Self::Identifier(r0, _)) => l0 == r0,
        }
    }
}

/// **Expressions** in the program source code.
#[derive(Debug, Clone)]
pub enum Ast {
    /// A literal is a constant value that is directly represented in the source code.
    /// 1. Value of the literal
    Literal { value: Value, info: LineInfo },
    /// A tuple is a fixed-size collection of elements of possibly different types.
    /// 1. List of elements
    Tuple { exprs: Vec<Ast>, info: LineInfo },
    /// A dynamic list of elements.
    /// 1. List of elements
    /// 2. Type of every element in the list (all elements must **be a subtype**)
    List { exprs: Vec<Ast>, info: LineInfo },
    /// A record is a collection of key-value pairs
    /// 1. List of key-value pairs
    Record {
        fields: Vec<(RecordKey, Ast)>,
        info: LineInfo,
    },
    /// An identifier is a named reference to a value in the environment
    /// 1. Name of the identifier
    Identifier { name: String, info: LineInfo },
    /// An assignment expression assigns a value to a variable
    /// 1. Matching pattern (identifier, destructuring of a tuple, record, etc.)
    /// 2. Value
    Assignment {
        target: Box<Ast>,
        expr: Box<Ast>,
        info: LineInfo,
    },
    /// A function declaration is a named function with a list of parameters and a body expression
    FunctionDef {
        param: ParamAst,
        body: Box<Ast>,
        return_type: Option<TypeAst>,
        info: LineInfo,
    },
    /// A function call is an invocation of a function with a list of arguments
    /// 1. Name of the function
    /// 2. List of arguments
    FunctionCall {
        expr: Box<Ast>,
        arg: Box<Ast>,
        info: LineInfo,
    },
    /// An accumulate expression is an operation with multiple operands
    /// 1. Operator
    /// 2. List of operands
    Accumulate {
        op_info: OperatorInfo,
        exprs: Vec<Ast>,
        info: LineInfo,
    },
    /// A binary expression is an operation with two operands
    /// 1. Left operand
    /// 2. Operator
    /// 3. Right operand
    Binary {
        lhs: Box<Ast>,
        op_info: OperatorInfo,
        rhs: Box<Ast>,
        info: LineInfo,
    },
    /// A unary expression is an operation with one operand
    /// 1. Operator
    /// 2. Operand
    Unary {
        op_info: OperatorInfo,
        expr: Box<Ast>,
        info: LineInfo,
    },
    /// Block expression evaluates all expressions in the block and returns the value of the last expression.
    /// 1. List of expressions
    Block(Vec<Ast>, LineInfo),
}

impl Ast {
    pub fn info(&self) -> &LineInfo {
        match self {
            Ast::Literal { info, .. } => info,
            Ast::Tuple { info, .. } => info,
            Ast::List { exprs: _, info } => info,
            Ast::Record { info, .. } => info,
            Ast::Identifier { info, .. } => info,
            Ast::FunctionCall {
                expr: _,
                arg: _,
                info,
            } => info,
            Ast::FunctionDef { info, .. } => info,
            Ast::Accumulate {
                op_info: _,
                exprs: _,
                info,
            } => info,
            Ast::Binary { info, .. } => info,
            Ast::Unary {
                op_info: _,
                expr: _,
                info,
            } => info,
            Ast::Assignment {
                target: _,
                expr: _,
                info,
            } => info,
            Ast::Block(_, info) => info,
        }
    }

    pub fn print_sexpr(&self) -> String {
        match self {
            Ast::Literal { value, .. } => value.pretty_print(),
            Ast::Tuple {
                exprs: elements, ..
            } => format!(
                "({})",
                elements
                    .iter()
                    .map(|e| e.print_sexpr())
                    .collect::<Vec<String>>()
                    .join(" ")
            ),
            Ast::List {
                exprs: elements,
                info: _,
            } => format!(
                "[{}]",
                elements
                    .iter()
                    .map(|e| e.print_sexpr())
                    .collect::<Vec<String>>()
                    .join(" ")
            ),
            Ast::Record {
                fields: _elements, ..
            } => todo!(),
            Ast::Identifier { name, .. } => name.clone(),
            Ast::FunctionCall { expr, arg, info: _ } => {
                format!("{}({})", expr.print_sexpr(), arg.print_sexpr())
            }
            Ast::FunctionDef {
                param: params,
                body,
                ..
            } => {
                if let Some(ty) = &params.ty {
                    format!(
                        "({} {} -> {})",
                        params.name,
                        ty.print_sexpr(),
                        body.print_sexpr()
                    )
                } else {
                    format!("(unknown {} -> {})", params.name, body.print_sexpr())
                }
            }
            Ast::Accumulate {
                op_info: op,
                exprs: operands,
                info: _,
            } => {
                format!(
                    "({} {})",
                    op.symbol.clone(),
                    operands
                        .iter()
                        .map(|e| e.print_sexpr())
                        .collect::<Vec<String>>()
                        .join(" ")
                )
            }
            Ast::Binary {
                lhs, op_info, rhs, ..
            } => format!(
                "({} {} {})",
                op_info.symbol.clone(),
                lhs.print_sexpr(),
                rhs.print_sexpr()
            ),
            Ast::Unary {
                op_info: op,
                expr: operand,
                info: _,
            } => {
                format!("({} {})", op.symbol.clone(), operand.print_sexpr())
            }
            Ast::Assignment {
                target: lhs,
                expr: rhs,
                info: _,
            } => {
                format!("(= {} {})", lhs.print_sexpr(), rhs.print_sexpr())
            }
            Ast::Block(expressions, _) => format!(
                "({})",
                expressions
                    .iter()
                    .map(|e| e.print_sexpr())
                    .collect::<Vec<String>>()
                    .join(" ")
            ),
        }
    }
}

impl PartialEq for Ast {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Literal { value: l0, .. }, Self::Literal { value: r0, .. }) => l0 == r0,
            (Self::Tuple { exprs: l0, .. }, Self::Tuple { exprs: r0, .. }) => l0 == r0,
            (Self::List { exprs: l0, info: _ }, Self::List { exprs: r0, info: _ }) => l0 == r0,
            (Self::Record { fields: l0, .. }, Self::Record { fields: r0, .. }) => l0 == r0,
            (Self::Identifier { name: name1, .. }, Self::Identifier { name: name2, .. }) => {
                name1 == name2
            }
            (
                Self::FunctionCall {
                    expr: l0,
                    arg: l1,
                    info: _,
                },
                Self::FunctionCall {
                    expr: r0,
                    arg: r1,
                    info: _,
                },
            ) => l0 == r0 && l1 == r1,
            (
                Self::FunctionDef {
                    param: l_param,
                    body: l_body,
                    return_type: l_return_type,
                    ..
                },
                Self::FunctionDef {
                    param: r_param,
                    body: r_body,
                    return_type: r_return_type,
                    ..
                },
            ) => l_param == r_param && l_body == r_body && l_return_type == r_return_type,
            (
                Self::Accumulate {
                    op_info: l0,
                    exprs: l1,
                    info: _,
                },
                Self::Accumulate {
                    op_info: r0,
                    exprs: r1,
                    info: _,
                },
            ) => l0 == r0 && l1 == r1,
            (
                Self::Binary {
                    rhs: rhs1,
                    op_info: op1,
                    lhs: lhs2,
                    ..
                },
                Self::Binary {
                    rhs: rhs2,
                    op_info: op2,
                    lhs: lhs1,
                    ..
                },
            ) => rhs1 == rhs2 && op1 == op2 && lhs2 == lhs1,
            (
                Self::Unary {
                    op_info: l0,
                    expr: l1,
                    info: _,
                },
                Self::Unary {
                    op_info: r0,
                    expr: r1,
                    info: _,
                },
            ) => l0 == r0 && l1 == r1,
            (
                Self::Assignment {
                    target: l0,
                    expr: l1,
                    info: _,
                },
                Self::Assignment {
                    target: r0,
                    expr: r1,
                    info: _,
                },
            ) => l0 == r0 && l1 == r1,
            (Self::Block(l0, _), Self::Block(r0, _)) => l0 == r0,
            _ => false,
        }
    }
}

/// Module is the root program node of the AST
/// It contains a list of all the expressions in the program
#[derive(Debug, Clone)]
pub struct Module {
    pub name: String,
    pub expressions: Vec<Ast>,
}

impl Module {
    pub fn new(name: String, expressions: Vec<Ast>) -> Module {
        Module { name, expressions }
    }
}
