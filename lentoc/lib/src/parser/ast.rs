use super::{op::OpInfo, pattern::BindPattern};
use crate::{
    interpreter::value::{RecordKey, Value},
    type_checker::checked_ast::TypeAst,
    util::error::LineInfo,
};
use std::fmt::Debug;

/// **Expressions** in the program source code.
#[derive(Clone)]
pub enum Ast {
    /// A literal is a constant value that is directly represented in the source code.
    Literal { value: Value, info: LineInfo },
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
    MemberAccess {
        /// The record expression to access the field from.
        expr: Box<Ast>,
        /// The field key to access.
        field: RecordKey,
        info: LineInfo,
    },
    /// An identifier is a named reference to a value in the environment.
    Identifier { name: String, info: LineInfo },
    /// A let expression binds a pattern to a value.
    Let {
        /// The target expression to assign to.
        target: BindPattern,
        /// The source expression to assign to the target.
        expr: Box<Ast>,
        /// Optional type annotation for the assigned variable.
        annotation: Option<TypeAst>,
        info: LineInfo,
    },
    /// A lambda expression is an anonymous function that can be passed as a value.
    Lambda {
        param: Box<Ast>,
        body: Box<Ast>,
        /// Optional return type annotation.
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
        op: OpInfo,
        rhs: Box<Ast>,
        info: LineInfo,
    },
    /// A unary expression is an operation with one operand.
    Unary {
        op: OpInfo,
        expr: Box<Ast>,
        info: LineInfo,
    },
    /// A block expression evaluates all expressions in the block and returns the value of the last expression.
    Block { exprs: Vec<Ast>, info: LineInfo },
    /// A function declaration (signature only, no body).
    /// e.g. `fn id :: a -> a`
    FunctionDecl {
        name: String,
        /// The type signature expression after `::`.
        signature: TypeAst,
        info: LineInfo,
    },
    /// A function definition (with body and optional return type/effects).
    /// e.g. `fn add(x, y) = x + y`, `fn foo(x) -> int ! { e } = x`
    FunctionDef {
        name: String,
        params: Vec<(BindPattern, Option<TypeAst>)>,
        /// Optional return type expression (after `->`, may include `! { effects }`).
        return_type: Option<Box<Ast>>,
        /// Optional pre-condition specification (`requires { ... }`) as code.
        requires: Option<Box<Ast>>,
        /// Optional post-condition specification (`ensures { ... }`) as code.
        ensures: Option<Box<Ast>>,
        body: Box<Ast>,
        info: LineInfo,
    },
    /// A type declaration.
    /// e.g. `type SmallIndex = u8`, `type Option(A) = Some(A) | None`
    TypeDecl {
        name: String,
        params: Vec<Ast>,
        body: TypeAst,
        info: LineInfo,
    },
}

impl Debug for Ast {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Literal { value, .. } => f.debug_struct("Literal").field("value", value).finish(),
            Self::Tuple { exprs, .. } => f.debug_struct("Tuple").field("exprs", exprs).finish(),
            Self::List { exprs, .. } => f.debug_struct("List").field("exprs", exprs).finish(),
            Self::Record { fields, .. } => {
                f.debug_struct("Record").field("fields", fields).finish()
            }
            Self::MemberAccess { expr, field, .. } => f
                .debug_struct("MemberAccess")
                .field("expr", expr)
                .field("field", field)
                .finish(),
            Self::Identifier { name, .. } => {
                f.debug_struct("Identifier").field("name", name).finish()
            }
            Self::Let {
                target,
                expr,
                annotation,
                ..
            } => f
                .debug_struct("Let")
                .field("target", target)
                .field("expr", expr)
                .field("annotation", annotation)
                .finish(),
            Self::Lambda {
                param,
                body,
                return_type,
                ..
            } => f
                .debug_struct("Lambda")
                .field("param", param)
                .field("body", body)
                .field("return_type", return_type)
                .finish(),
            Self::FunctionCall { expr, arg, .. } => f
                .debug_struct("FunctionCall")
                .field("expr", expr)
                .field("arg", arg)
                .finish(),
            Self::Binary {
                lhs,
                op: op_info,
                rhs,
                ..
            } => f
                .debug_struct("Binary")
                .field("lhs", lhs)
                .field("op_info", op_info)
                .field("rhs", rhs)
                .finish(),
            Self::Unary {
                op: op_info, expr, ..
            } => f
                .debug_struct("Unary")
                .field("op_info", op_info)
                .field("expr", expr)
                .finish(),
            Self::Block { exprs, .. } => f.debug_struct("Block").field("exprs", exprs).finish(),
            Self::FunctionDecl {
                name, signature, ..
            } => f
                .debug_struct("FunctionDecl")
                .field("name", name)
                .field("signature", signature)
                .finish(),
            Self::FunctionDef {
                name,
                params,
                return_type,
                requires,
                ensures,
                body,
                ..
            } => f
                .debug_struct("FunctionDef")
                .field("name", name)
                .field("params", params)
                .field("return_type", return_type)
                .field("requires", requires)
                .field("ensures", ensures)
                .field("body", body)
                .finish(),
            Self::TypeDecl {
                name, params, body, ..
            } => f
                .debug_struct("TypeDecl")
                .field("name", name)
                .field("params", params)
                .field("body", body)
                .finish(),
        }
    }
}

impl Ast {
    pub fn unit(info: LineInfo) -> Self {
        Ast::Tuple {
            exprs: vec![],
            info,
        }
    }

    pub fn info(&self) -> &LineInfo {
        match self {
            Ast::Literal { info, .. } => info,
            Ast::Tuple { info, .. } => info,
            Ast::List { info, .. } => info,
            Ast::Record { info, .. } => info,
            Ast::MemberAccess { info, .. } => info,
            Ast::Identifier { info, .. } => info,
            Ast::FunctionCall { info, .. } => info,
            Ast::Lambda { info, .. } => info,
            Ast::Binary { info, .. } => info,
            Ast::Unary { info, .. } => info,
            Ast::Let { info, .. } => info,
            Ast::Block { info, .. } => info,
            Ast::FunctionDecl { info, .. } => info,
            Ast::FunctionDef { info, .. } => info,
            Ast::TypeDecl { info, .. } => info,
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
            Ast::MemberAccess { expr, field, .. } => format!("({}.{})", expr.print_expr(), field),
            Ast::Identifier { name, .. } => name.clone(),
            Ast::FunctionCall { expr, arg, info: _ } => {
                format!("({} {})", expr.print_expr(), arg.print_expr())
            }
            Ast::Lambda { param, body, .. } => {
                format!("({} => {})", param.print_expr(), body.print_expr())
            }

            Ast::Binary {
                lhs,
                op: op_info,
                rhs,
                ..
            } => format!(
                "({} {} {})",
                lhs.print_expr(),
                op_info.symbol.clone(),
                rhs.print_expr()
            ),
            Ast::Unary {
                op, expr: operand, ..
            } => {
                format!("({} {})", op.symbol.clone(), operand.print_expr())
            }
            Ast::Let {
                target: lhs,
                expr: rhs,
                annotation,
                ..
            } => {
                let ann = annotation
                    .as_ref()
                    .map(|a| format!("{} ", a.pretty_print()))
                    .unwrap_or_default();
                format!("({}{} = {})", ann, lhs.print_expr(), rhs.print_expr())
            }
            Ast::Block { exprs, .. } => format!(
                "{{ {} }}",
                exprs
                    .iter()
                    .map(|e| e.print_expr())
                    .collect::<Vec<String>>()
                    .join("; ")
            ),
            Ast::FunctionDecl {
                name, signature, ..
            } => {
                format!("fn {} :: {}", name, signature.pretty_print())
            }
            Ast::FunctionDef {
                name,
                params,
                return_type,
                requires,
                ensures,
                body,
                ..
            } => {
                let params_str = params
                    .iter()
                    .map(|(p, ty)| match ty {
                        Some(ty) => format!("{}: {}", p.print_expr(), ty.pretty_print()),
                        None => p.print_expr(),
                    })
                    .collect::<Vec<String>>()
                    .join(", ");
                let ret = return_type
                    .as_ref()
                    .map(|r| format!(" -> {}", r.print_expr()))
                    .unwrap_or_default();
                let req = requires
                    .as_ref()
                    .map(|r| format!(" requires {{ {} }}", r.print_expr()))
                    .unwrap_or_default();
                let ens = ensures
                    .as_ref()
                    .map(|e| format!(" ensures {{ {} }}", e.print_expr()))
                    .unwrap_or_default();
                format!(
                    "fn {}({}){}{}{} = {}",
                    name,
                    params_str,
                    ret,
                    req,
                    ens,
                    body.print_expr()
                )
            }
            Ast::TypeDecl {
                name, params, body, ..
            } => {
                let params_str = if params.is_empty() {
                    String::new()
                } else {
                    format!(
                        "({})",
                        params
                            .iter()
                            .map(|p| p.print_expr())
                            .collect::<Vec<String>>()
                            .join(", ")
                    )
                };
                format!("type {}{} = {}", name, params_str, body.print_expr())
            }
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
                Self::Binary {
                    rhs: rhs1,
                    op: op1,
                    lhs: lhs2,
                    ..
                },
                Self::Binary {
                    rhs: rhs2,
                    op: op2,
                    lhs: lhs1,
                    ..
                },
            ) => rhs1 == rhs2 && op1 == op2 && lhs2 == lhs1,
            (
                Self::Unary {
                    op: l0, expr: l1, ..
                },
                Self::Unary {
                    op: r0, expr: r1, ..
                },
            ) => l0 == r0 && l1 == r1,
            (
                Self::Let {
                    target: l1,
                    expr: l2,
                    annotation: l3,
                    ..
                },
                Self::Let {
                    target: r1,
                    expr: r2,
                    annotation: r3,
                    ..
                },
            ) => l1 == r1 && l2 == r2 && l3 == r3,
            (
                Self::Lambda {
                    param: l_param,
                    body: l_body,
                    return_type: l_ret,
                    ..
                },
                Self::Lambda {
                    param: r_param,
                    body: r_body,
                    return_type: r_ret,
                    ..
                },
            ) => l_param == r_param && l_body == r_body && l_ret == r_ret,
            (Self::Block { exprs: l0, .. }, Self::Block { exprs: r0, .. }) => l0 == r0,
            (
                Self::FunctionDecl {
                    name: l0,
                    signature: l1,
                    ..
                },
                Self::FunctionDecl {
                    name: r0,
                    signature: r1,
                    ..
                },
            ) => l0 == r0 && l1 == r1,
            (
                Self::FunctionDef {
                    name: l0,
                    params: l1,
                    return_type: l2,
                    requires: l3,
                    ensures: l4,
                    body: l5,
                    ..
                },
                Self::FunctionDef {
                    name: r0,
                    params: r1,
                    return_type: r2,
                    requires: r3,
                    ensures: r4,
                    body: r5,
                    ..
                },
            ) => l0 == r0 && l1 == r1 && l2 == r2 && l3 == r3 && l4 == r4 && l5 == r5,
            (
                Self::TypeDecl {
                    name: l0,
                    params: l1,
                    body: l2,
                    ..
                },
                Self::TypeDecl {
                    name: r0,
                    params: r1,
                    body: r2,
                    ..
                },
            ) => l0 == r0 && l1 == r1 && l2 == r2,
            _ => false,
        }
    }
}
