use crate::{
    interpreter::value::{RecordKey, Value},
    util::error::LineInfo,
};

use super::{op::OpInfo, pattern::BindPattern};

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
    Identifier {
        /// The name of the type.
        name: String,
        /// The line information for the type.
        info: LineInfo,
    },
    Constructor {
        expr: Box<TypeAst>,
        args: Vec<TypeAst>,
        info: LineInfo,
    },
}

impl TypeAst {
    pub fn info(&self) -> &LineInfo {
        match self {
            TypeAst::Identifier { info, .. } => info,
            TypeAst::Constructor { info, .. } => info,
        }
    }

    pub fn print_expr(&self) -> String {
        match self {
            TypeAst::Identifier { name, .. } => name.clone(),
            TypeAst::Constructor { expr, args, .. } => {
                format!(
                    "{}({})",
                    expr.print_expr(),
                    args.iter()
                        .map(|a| a.print_expr())
                        .collect::<Vec<String>>()
                        .join(", ")
                )
            }
        }
    }

    pub fn pretty_print(&self) -> String {
        match self {
            TypeAst::Identifier { name, .. } => name.clone(),
            TypeAst::Constructor { expr, args, .. } => {
                format!(
                    "{}({})",
                    expr.pretty_print(),
                    args.iter()
                        .map(|a| a.pretty_print())
                        .collect::<Vec<String>>()
                        .join(", ")
                )
            }
        }
    }
}

impl PartialEq for TypeAst {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Identifier { name: l0, .. }, Self::Identifier { name: r0, .. }) => l0 == r0,
            (
                Self::Constructor {
                    expr: l0,
                    args: l1,
                    info: _,
                },
                Self::Constructor {
                    expr: r0,
                    args: r1,
                    info: _,
                },
            ) => l0 == r0 && l1 == r1,
            _ => false,
        }
    }
}

/// **Expressions** in the program source code.
#[derive(Debug, Clone)]
pub enum Ast {
    /// A literal is a constant value that is directly represented in the source code.
    Literal { value: Value, info: LineInfo },
    /// A literal type expression
    LiteralType {
        /// The literal type value.
        expr: TypeAst,
    },
    /// A tuple is a fixed-size collection of elements of possibly different types.
    Tuple { exprs: Vec<Ast>, info: LineInfo },
    /// A dynamic list of elements.
    List { exprs: Vec<Ast>, info: LineInfo },
    /// A record is a collection of key-value fields.
    Record {
        fields: Vec<(RecordKey, Ast)>,
        info: LineInfo,
    },
    /// A member field access expression is a reference to a field in a record.
    MemderAccess {
        /// The record expression to access the field from.
        expr: Box<Ast>,
        /// The field key to access.
        field: RecordKey,
        info: LineInfo,
    },
    /// An identifier is a named reference to a value in the environment.
    Identifier { name: String, info: LineInfo },
    /// An assignment expression assigns a value to a variable via a matching pattern (identifier, destructuring of a tuple, record, etc.).
    Assignment {
        /// Any type annotation for the target expression.
        annotation: Option<TypeAst>,
        /// The target expression to assign to.
        target: BindPattern,
        /// The source expression to assign to the target.
        expr: Box<Ast>,
        info: LineInfo,
    },
    /// A lambda expression is an anonymous function that can be passed as a value.
    Lambda {
        param: ParamAst,
        body: Box<Ast>,
        return_type: Option<TypeAst>,
        info: LineInfo,
    },
    /// A function call is an invocation of a function with a list of arguments.
    FunctionCall {
        expr: Box<Ast>,
        arg: Box<Ast>,
        info: LineInfo,
    },
    /// A binary expression is an operation with two operands.
    Binary {
        lhs: Box<Ast>,
        op_info: OpInfo,
        rhs: Box<Ast>,
        info: LineInfo,
    },
    /// A unary expression is an operation with one operand.
    Unary {
        op_info: OpInfo,
        expr: Box<Ast>,
        info: LineInfo,
    },
    /// Block expression evaluates all expressions in the block and returns the value of the last expression.
    Block { exprs: Vec<Ast>, info: LineInfo },
}

impl Ast {
    pub fn info(&self) -> &LineInfo {
        match self {
            Ast::Literal { info, .. } => info,
            Ast::Tuple { info, .. } => info,
            Ast::List { info, .. } => info,
            Ast::Record { info, .. } => info,
            Ast::MemderAccess { info, .. } => info,
            Ast::LiteralType { expr, .. } => expr.info(),
            Ast::Identifier { info, .. } => info,
            Ast::FunctionCall { info, .. } => info,
            Ast::Lambda { info, .. } => info,
            Ast::Binary { info, .. } => info,
            Ast::Unary { info, .. } => info,
            Ast::Assignment { info, .. } => info,
            Ast::Block { info, .. } => info,
        }
    }

    pub fn last_info(&self) -> &LineInfo {
        match self {
            Ast::Block { exprs, .. } => exprs.last().map_or_else(|| self.info(), |e| e.last_info()),
            _ => self.info(),
        }
    }

    pub fn print_expr(&self) -> String {
        match self {
            Ast::Literal { value, .. } => value.pretty_print(),
            Ast::LiteralType { expr, .. } => expr.print_expr(),
            Ast::Tuple {
                exprs: elements, ..
            } => format!(
                "({})",
                elements
                    .iter()
                    .map(|e| e.print_expr())
                    .collect::<Vec<String>>()
                    .join(", ")
            ),
            Ast::List {
                exprs: elements, ..
            } => format!(
                "[{}]",
                elements
                    .iter()
                    .map(|e| e.print_expr())
                    .collect::<Vec<String>>()
                    .join(", ")
            ),
            Ast::Record { fields, .. } => format!(
                "{{ {} }}",
                fields
                    .iter()
                    .map(|(k, v)| format!("{}: {}", k, v.print_expr()))
                    .collect::<Vec<String>>()
                    .join(", ")
            ),
            Ast::MemderAccess { expr, field, .. } => format!("({}.{})", expr.print_expr(), field),
            Ast::Identifier { name, .. } => name.clone(),
            Ast::FunctionCall { expr, arg, info: _ } => {
                format!("({} {})", expr.print_expr(), arg.print_expr())
            }
            Ast::Lambda {
                param: params,
                body,
                ..
            } => {
                if let Some(ty) = &params.ty {
                    format!(
                        "({} {} -> {})",
                        ty.print_expr(),
                        params.name,
                        body.print_expr()
                    )
                } else {
                    format!("({} -> {})", params.name, body.print_expr())
                }
            }

            Ast::Binary {
                lhs, op_info, rhs, ..
            } => format!(
                "({} {} {})",
                lhs.print_expr(),
                op_info.symbol.clone(),
                rhs.print_expr()
            ),
            Ast::Unary {
                op_info: op,
                expr: operand,
                ..
            } => {
                format!("({} {})", op.symbol.clone(), operand.print_expr())
            }
            Ast::Assignment {
                annotation: ty,
                target: lhs,
                expr: rhs,
                ..
            } => {
                if let Some(ty) = ty {
                    format!(
                        "({} {} = {})",
                        ty.print_expr(),
                        lhs.print_expr(),
                        rhs.print_expr()
                    )
                } else {
                    format!("({} = {})", lhs.print_expr(), rhs.print_expr())
                }
            }
            Ast::Block { exprs, .. } => format!(
                "{{ {} }}",
                exprs
                    .iter()
                    .map(|e| e.print_expr())
                    .collect::<Vec<String>>()
                    .join("; ")
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
                    expr: l0, arg: l1, ..
                },
                Self::FunctionCall {
                    expr: r0, arg: r1, ..
                },
            ) => l0 == r0 && l1 == r1,
            (
                Self::Lambda {
                    param: l_param,
                    body: l_body,
                    return_type: l_return_type,
                    ..
                },
                Self::Lambda {
                    param: r_param,
                    body: r_body,
                    return_type: r_return_type,
                    ..
                },
            ) => l_param == r_param && l_body == r_body && l_return_type == r_return_type,
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
                    ..
                },
                Self::Unary {
                    op_info: r0,
                    expr: r1,
                    ..
                },
            ) => l0 == r0 && l1 == r1,
            (
                Self::Assignment {
                    annotation: l0,
                    target: l1,
                    expr: l2,
                    ..
                },
                Self::Assignment {
                    annotation: r0,
                    target: r1,
                    expr: r2,
                    ..
                },
            ) => l0 == r0 && l1 == r1 && l2 == r2,
            (Self::Block { exprs: l0, .. }, Self::Block { exprs: r0, .. }) => l0 == r0,
            _ => false,
        }
    }
}
