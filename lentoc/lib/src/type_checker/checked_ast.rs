use super::types::{std_types, FunctionType, GetType, TypeJudgements, TypeTrait};
use crate::{
    interpreter::value::{Function, RecordKey, Value},
    parser::ast::Ast,
    parser::pattern::BindPattern,
    type_checker::types::Type,
    util::error::LineInfo,
};
use std::fmt::Debug;

#[derive(Debug, Clone)]
pub struct ParamAst {
    pub ty: Option<TypeAst>,
    pub pattern: BindPattern,
}

impl PartialEq for ParamAst {
    fn eq(&self, other: &Self) -> bool {
        self.pattern == other.pattern && self.ty == other.ty
    }
}

#[derive(Clone)]
pub enum TypeAst {
    Identifier {
        name: String,
        info: LineInfo,
    },
    Application {
        expr: Box<TypeAst>,
        args: Vec<TypeAst>,
        info: LineInfo,
    },
    Tuple {
        items: Vec<TypeAst>,
        info: LineInfo,
    },
    StaticVector {
        elem: Box<TypeAst>,
        len: usize,
        info: LineInfo,
    },
    Record {
        fields: Vec<(RecordKey, TypeAst)>,
        info: LineInfo,
    },
    Refinement {
        binder: RecordKey,
        base: Box<TypeAst>,
        predicate: Box<Ast>,
        info: LineInfo,
    },
    Sum {
        variants: Vec<TypeAst>,
        info: LineInfo,
    },
    Lambda {
        lhs: Box<TypeAst>,
        rhs: Box<TypeAst>,
        eff: Option<Box<TypeAst>>,
        info: LineInfo,
    },
    Literal {
        value: Value,
        info: LineInfo,
    },
}

impl Debug for TypeAst {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Identifier { name, .. } => {
                f.debug_struct("Identifier").field("name", name).finish()
            }
            Self::Application { expr, args, .. } => f
                .debug_struct("Application")
                .field("expr", expr)
                .field("args", args)
                .finish(),
            Self::Tuple { items, .. } => f.debug_struct("Tuple").field("items", items).finish(),
            Self::StaticVector { elem, len, .. } => f
                .debug_struct("StaticVector")
                .field("elem", elem)
                .field("len", len)
                .finish(),
            Self::Record { fields, .. } => {
                f.debug_struct("Record").field("fields", fields).finish()
            }
            Self::Refinement {
                binder,
                base,
                predicate,
                ..
            } => f
                .debug_struct("Refinement")
                .field("binder", binder)
                .field("base", base)
                .field("predicate", predicate)
                .finish(),
            Self::Sum { variants, .. } => {
                f.debug_struct("Sum").field("variants", variants).finish()
            }
            Self::Lambda { lhs, rhs, eff, .. } => f
                .debug_struct("Lambda")
                .field("lhs", lhs)
                .field("rhs", rhs)
                .field("eff", eff)
                .finish(),
            Self::Literal { value, .. } => f.debug_struct("Literal").field("value", value).finish(),
        }
    }
}

impl TypeAst {
    pub fn info(&self) -> &LineInfo {
        match self {
            TypeAst::Identifier { info, .. } => info,
            TypeAst::Application { info, .. } => info,
            TypeAst::Tuple { info, .. } => info,
            TypeAst::StaticVector { info, .. } => info,
            TypeAst::Record { info, .. } => info,
            TypeAst::Refinement { info, .. } => info,
            TypeAst::Sum { info, .. } => info,
            TypeAst::Lambda { info, .. } => info,
            TypeAst::Literal { info, .. } => info,
        }
    }

    pub fn print_expr(&self) -> String {
        match self {
            TypeAst::Identifier { name, .. } => name.clone(),
            TypeAst::Application {
                expr, args, ..
            } => {
                format!(
                    "{}({})",
                    expr.print_expr(),
                    args.iter()
                        .map(|a| a.print_expr())
                        .collect::<Vec<String>>()
                        .join(", ")
                )
            }
            TypeAst::Tuple { items, .. } => {
                if items.is_empty() {
                    "()".to_string()
                } else {
                    format!(
                        "({})",
                        items
                            .iter()
                            .map(|a| a.print_expr())
                            .collect::<Vec<String>>()
                            .join(", ")
                    )
                }
            }
            TypeAst::StaticVector { elem, len, .. } => {
                format!("[{}; {}]", elem.print_expr(), len)
            }
            TypeAst::Record { fields, .. } => {
                format!(
                    "{{ {} }}",
                    fields
                        .iter()
                        .map(|(k, v)| format!("{}: {}", k, v.print_expr()))
                        .collect::<Vec<String>>()
                        .join(", ")
                )
            }
            TypeAst::Refinement {
                binder,
                base,
                predicate,
                ..
            } => {
                format!("{{ {}: {} | {} }}", binder, base.print_expr(), predicate.print_expr())
            }
            TypeAst::Sum { variants, .. } => {
                format!(
                    "({})",
                    variants
                        .iter()
                        .map(|t| t.print_expr())
                        .collect::<Vec<String>>()
                        .join(" | ")
                )
            }
            TypeAst::Lambda { lhs, rhs, eff, .. } => {
                if let Some(eff) = eff {
                    format!(
                        "({} -> {} ! {})",
                        lhs.print_expr(),
                        rhs.print_expr(),
                        eff.print_expr()
                    )
                } else {
                    format!("({} -> {})", lhs.print_expr(), rhs.print_expr())
                }
            }
            TypeAst::Literal { value, .. } => value.pretty_print(),
        }
    }

    pub fn pretty_print(&self) -> String {
        match self {
            TypeAst::Identifier { name, .. } => name.clone(),
            TypeAst::Application {
                expr, args, ..
            } => {
                format!(
                    "{}({})",
                    expr.pretty_print(),
                    args.iter()
                        .map(|a| a.pretty_print())
                        .collect::<Vec<String>>()
                        .join(", ")
                )
            }
            TypeAst::Tuple { items, .. } => {
                if items.is_empty() {
                    "()".to_string()
                } else {
                    format!(
                        "({})",
                        items
                            .iter()
                            .map(|a| a.pretty_print())
                            .collect::<Vec<String>>()
                            .join(", ")
                    )
                }
            }
            TypeAst::StaticVector { elem, len, .. } => {
                format!("[{}; {}]", elem.pretty_print(), len)
            }
            TypeAst::Record { fields, .. } => {
                format!(
                    "{{ {} }}",
                    fields
                        .iter()
                        .map(|(k, v)| format!("{}: {}", k, v.pretty_print()))
                        .collect::<Vec<String>>()
                        .join(", ")
                )
            }
            TypeAst::Refinement {
                binder,
                base,
                predicate,
                ..
            } => {
                format!("{{ {}: {} | {} }}", binder, base.pretty_print(), predicate.print_expr())
            }
            TypeAst::Sum { variants, .. } => {
                format!(
                    "({})",
                    variants
                        .iter()
                        .map(|t| t.pretty_print())
                        .collect::<Vec<String>>()
                        .join(" | ")
                )
            }
            TypeAst::Lambda { lhs, rhs, eff, .. } => {
                if let Some(eff) = eff {
                    format!(
                        "({} -> {} ! {})",
                        lhs.pretty_print(),
                        rhs.pretty_print(),
                        eff.pretty_print()
                    )
                } else {
                    format!("({} -> {})", lhs.pretty_print(), rhs.pretty_print())
                }
            }
            TypeAst::Literal { value, .. } => value.pretty_print(),
        }
    }
}

impl PartialEq for TypeAst {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Identifier { name: l0, .. }, Self::Identifier { name: r0, .. }) => l0 == r0,
            (
                Self::Application {
                    expr: l0,
                    args: l1,
                    info: _,
                },
                Self::Application {
                    expr: r0,
                    args: r1,
                    info: _,
                },
            ) => l0 == r0 && l1 == r1,
            (
                Self::Tuple {
                    items: l0,
                    info: _,
                },
                Self::Tuple {
                    items: r0,
                    info: _,
                },
            ) => l0 == r0,
            (
                Self::StaticVector {
                    elem: l0,
                    len: l1,
                    info: _,
                },
                Self::StaticVector {
                    elem: r0,
                    len: r1,
                    info: _,
                },
            ) => l0 == r0 && l1 == r1,
            (
                Self::Record {
                    fields: l0,
                    info: _,
                },
                Self::Record {
                    fields: r0,
                    info: _,
                },
            ) => l0 == r0,
            (
                Self::Refinement {
                    binder: l0,
                    base: l1,
                    predicate: l2,
                    info: _,
                },
                Self::Refinement {
                    binder: r0,
                    base: r1,
                    predicate: r2,
                    info: _,
                },
            ) => l0 == r0 && l1 == r1 && l2 == r2,
            (
                Self::Sum {
                    variants: l0,
                    info: _,
                },
                Self::Sum {
                    variants: r0,
                    info: _,
                },
            ) => l0 == r0,
            (
                Self::Lambda {
                    lhs: l0,
                    rhs: l1,
                    eff: l2,
                    info: _,
                },
                Self::Lambda {
                    lhs: r0,
                    rhs: r1,
                    eff: r2,
                    info: _,
                },
            ) => l0 == r0 && l1 == r1 && l2 == r2,
            (Self::Literal { value: l0, .. }, Self::Literal { value: r0, .. }) => l0 == r0,
            _ => false,
        }
    }
}

#[derive(Debug, Clone)]
pub struct CheckedOperator {
    pub name: String,
    pub symbol: String,
    pub handler: Function,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct CheckedParam {
    pub ty: Type,
    pub pattern: BindPattern,
}

impl CheckedParam {
    pub fn new(pattern: BindPattern, ty: Type) -> CheckedParam {
        CheckedParam { pattern, ty }
    }

    pub fn from_str<S: Into<String>>(name: S, ty: Type) -> CheckedParam {
        CheckedParam::new(
            BindPattern::Variable {
                name: name.into(),
                info: LineInfo::default(),
            },
            ty,
        )
    }
}

/// The AST is a tree of nodes that represent the program.
/// All nodes are expressions, and the root node is the program itself.
/// The AST is generated by the parser, and then interpreted by the interpreter module or compiled.
#[derive(Debug, Clone)]
pub enum CheckedAst {
    /// A literal is a constant value that is directly represented in the source code.
    Literal { value: Value, info: LineInfo },
    /// A tuple is a fixed-size collection of elements of possibly different types.
    Tuple {
        exprs: Vec<CheckedAst>,
        /// The type of the tuple, made up of the types of the elements.
        /// Each element's type is listed in the same order as the elements.
        ty: Type,
        info: LineInfo,
    },
    /// A dynamic list of elements.
    List {
        exprs: Vec<CheckedAst>,
        /// The type of the list, a sum type of all the types of the elements.
        /// All elements **must be a subtype** of this type.
        ty: Type,
        info: LineInfo,
    },
    /// A record is a collection of key-value pairs.
    Record {
        fields: Vec<(RecordKey, CheckedAst)>,
        /// The type of the record, made up of the types of the keys and values.
        ty: Type,
        info: LineInfo,
    },
    /// A field access expression is a reference to a field in a record.
    FieldAccess {
        /// The record expression to access the field from.
        expr: Box<CheckedAst>,
        /// The key of the field to access.
        field: RecordKey,
        /// The type of the field, the type of the value in the record.
        ty: Type,
        info: LineInfo,
    },
    /// An identifier is a named reference to a value in the environment.
    Identifier {
        name: String,
        /// Type of the identifier (the type of the value it refers to)
        ty: Type,
        info: LineInfo,
    },
    /// A function call is an invocation of an expression with a specific argument.
    FunctionCall {
        /// Expression body of the function (identifier, lambda, etc.)
        expr: Box<CheckedAst>,
        /// The argument to the function call
        arg: Box<CheckedAst>,
        /// The return type of the function call
        ret_ty: Type,
        info: LineInfo,
    },
    /// A lambda expression is an anonymous function that can be passed as a value.
    Lambda {
        /// The parameter of the lambda function
        param: CheckedParam,
        /// The body of the lambda function
        body: Box<CheckedAst>,
        /// The return type of the lambda function
        return_type: Type,
        /// The type of the lambda function, which is a function type
        ty: Type,
        info: LineInfo,
    },
    /// A let expression binds a value to a matching pattern (identifier, tuple, record, etc.).
    Let {
        target: BindPattern,
        expr: Box<CheckedAst>,
        info: LineInfo,
    },
    /// Block expression evaluates all expressions in the block and returns the value of the last expression.
    /// 1. List of expressions
    /// 2. Type of the last expression
    Block {
        exprs: Vec<CheckedAst>,
        ty: Type,
        info: LineInfo,
    },
    /// A function declaration (signature only, no body).
    /// e.g. `fn id :: a -> a`
    FunctionDecl {
        name: String,
        sig_type: Type,
        info: LineInfo,
    },
    /// A function definition (with body and optional return type/effects).
    /// e.g. `fn add(x, y) = x + y`, `fn foo(x) -> int ! { e } = x`
    FunctionDef {
        name: String,
        params: Vec<CheckedParam>,
        return_type: Option<Type>,
        requires: Option<Box<CheckedAst>>,
        ensures: Option<Box<CheckedAst>>,
        body: Box<CheckedAst>,
        info: LineInfo,
    },
    /// A type declaration.
    /// e.g. `type SmallIndex = u8`, `type Option(A) = Some(A) | None`
    TypeDecl { name: String, info: LineInfo },
}

impl GetType for CheckedAst {
    fn get_type(&self) -> &Type {
        match self {
            CheckedAst::Literal { value: v, info: _ } => v.get_type(),
            CheckedAst::Tuple { ty, .. } => ty,
            CheckedAst::List { ty, .. } => ty,
            CheckedAst::Record { ty, .. } => ty,
            CheckedAst::FieldAccess { ty, .. } => ty,
            CheckedAst::Identifier { ty, .. } => ty,
            CheckedAst::FunctionCall { ret_ty, .. } => ret_ty,
            CheckedAst::Lambda { ty, .. } => ty,
            CheckedAst::Let { .. } => &std_types::UNIT,
            CheckedAst::Block { exprs: _, ty, .. } => ty,
            CheckedAst::FunctionDecl { sig_type, .. } => sig_type,
            CheckedAst::FunctionDef { .. } => &std_types::UNIT,
            CheckedAst::TypeDecl { .. } => &std_types::UNIT,
        }
    }
}

impl CheckedAst {
    pub fn unit(info: LineInfo) -> CheckedAst {
        CheckedAst::Tuple {
            exprs: vec![],
            ty: std_types::UNIT,
            info,
        }
    }

    pub fn lambda(
        param: CheckedParam,
        body: CheckedAst,
        return_type: Type,
        info: LineInfo,
    ) -> CheckedAst {
        CheckedAst::Lambda {
            ty: Type::Function(Box::new(FunctionType::new(
                param.clone(),
                return_type.clone(),
            ))),
            param,
            body: Box::new(body),
            return_type,
            info,
        }
    }

    pub fn info(&self) -> &LineInfo {
        match self {
            CheckedAst::Literal { info, .. } => info,
            CheckedAst::Tuple { info, .. } => info,
            CheckedAst::List { info, .. } => info,
            CheckedAst::Record { info, .. } => info,
            CheckedAst::FieldAccess { info, .. } => info,
            CheckedAst::Identifier { info, .. } => info,
            CheckedAst::FunctionCall { info, .. } => info,
            CheckedAst::Lambda { info, .. } => info,
            CheckedAst::Let { info, .. } => info,
            CheckedAst::Block { info, .. } => info,
            CheckedAst::FunctionDecl { info, .. } => info,
            CheckedAst::FunctionDef { info, .. } => info,
            CheckedAst::TypeDecl { info, .. } => info,
        }
    }

    pub fn specialize(&mut self, judgements: &TypeJudgements, changed: &mut bool) {
        match self {
            CheckedAst::Literal { .. } => (),
            CheckedAst::Tuple {
                exprs: elements,
                ty,
                ..
            } => {
                for element in elements {
                    element.specialize(judgements, changed);
                }
                *ty = ty.specialize(judgements, changed);
            }
            CheckedAst::List {
                exprs: elements,
                ty,
                ..
            } => {
                for element in elements {
                    element.specialize(judgements, changed);
                }
                *ty = ty.specialize(judgements, changed);
            }
            CheckedAst::Record {
                fields: elements,
                ty,
                ..
            } => {
                for (_, element) in elements {
                    element.specialize(judgements, changed);
                }
                *ty = ty.specialize(judgements, changed);
            }
            CheckedAst::FieldAccess {
                expr: record,
                field: _,
                ty,
                ..
            } => {
                record.specialize(judgements, changed);
                *ty = ty.specialize(judgements, changed);
            }
            CheckedAst::Identifier { name: _, ty, .. } => {
                *ty = ty.specialize(judgements, changed);
            }
            CheckedAst::FunctionCall {
                expr: function,
                arg,
                ret_ty: return_type,
                ..
            } => {
                function.specialize(judgements, changed);
                arg.specialize(judgements, changed);
                *return_type = return_type.specialize(judgements, changed);
            }
            CheckedAst::Lambda {
                param,
                body,
                return_type,
                ty,
                ..
            } => {
                param.ty = param.ty.specialize(judgements, changed);
                *return_type = return_type.specialize(judgements, changed);
                *ty = ty.specialize(judgements, changed);
                body.specialize(judgements, changed);
            }
            CheckedAst::Let {
                target: lhs,
                expr: rhs,
                ..
            } => {
                lhs.specialize(judgements, changed);
                rhs.specialize(judgements, changed);
            }
            CheckedAst::Block {
                exprs: expressions,
                ty,
                ..
            } => {
                for expression in expressions {
                    expression.specialize(judgements, changed);
                }
                *ty = ty.specialize(judgements, changed);
            }
            CheckedAst::FunctionDecl { sig_type, .. } => {
                *sig_type = sig_type.specialize(judgements, changed);
            }
            CheckedAst::FunctionDef {
                params,
                return_type,
                requires,
                ensures,
                body,
                ..
            } => {
                for param in params.iter_mut() {
                    param.ty = param.ty.specialize(judgements, changed);
                }
                if let Some(ret) = return_type {
                    *ret = ret.specialize(judgements, changed);
                }
                if let Some(req) = requires {
                    req.specialize(judgements, changed);
                }
                if let Some(ens) = ensures {
                    ens.specialize(judgements, changed);
                }
                body.specialize(judgements, changed);
            }
            CheckedAst::TypeDecl { .. } => (),
        }
    }

    pub fn print_expr(&self) -> String {
        match self {
            CheckedAst::Literal { value, info: _ } => value.pretty_print(),
            CheckedAst::Tuple {
                exprs: elements, ..
            } => format!(
                "({})",
                elements
                    .iter()
                    .map(|e| e.print_expr())
                    .collect::<Vec<String>>()
                    .join(", ")
            ),
            CheckedAst::List {
                exprs: elements, ..
            } => format!(
                "[{}]",
                elements
                    .iter()
                    .map(|e| e.print_expr())
                    .collect::<Vec<String>>()
                    .join(", ")
            ),
            CheckedAst::Record { fields, .. } => format!(
                "{{ {} }}",
                fields
                    .iter()
                    .map(|(k, v)| format!("{}: {}", k, v.print_expr()))
                    .collect::<Vec<String>>()
                    .join(", ")
            ),
            CheckedAst::FieldAccess {
                expr: record,
                field,
                ..
            } => format!("({}.{})", record.print_expr(), field),
            CheckedAst::Identifier { name, .. } => name.clone(),
            CheckedAst::FunctionCall {
                expr: function,
                arg,
                ..
            } => {
                // format!("{}({})", function.print_sexpr(), arg.print_sexpr())

                // Unwrap any nested calls to print the full call chain as "f(x, y, z)"
                let mut function = function;
                let mut args = vec![arg];
                while let CheckedAst::FunctionCall {
                    expr: f, arg: a, ..
                } = &**function
                {
                    function = f;
                    // All nested applications are performed before the current one
                    args.insert(0, a);
                }
                format!(
                    "{}({})",
                    function.print_expr(),
                    args.iter()
                        .map(|a| a.print_expr())
                        .collect::<Vec<String>>()
                        .join(", ")
                )
            }
            CheckedAst::Lambda { param, body, .. } => {
                format!(
                    "({} {} -> {})",
                    param.ty,
                    param.pattern.print_expr(),
                    body.print_expr()
                )
            }
            CheckedAst::Let {
                target: lhs,
                expr: rhs,
                ..
            } => {
                format!("({} = {})", lhs.print_expr(), rhs.print_expr())
            }
            CheckedAst::Block {
                exprs: expressions, ..
            } => format!(
                "{{ {} }}",
                expressions
                    .iter()
                    .map(|e| e.print_expr())
                    .collect::<Vec<String>>()
                    .join("; ")
            ),
            CheckedAst::FunctionDecl { name, sig_type, .. } => {
                format!("fn {} :: {}", name, sig_type)
            }
            CheckedAst::FunctionDef {
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
                    .map(|p| p.pattern.print_expr())
                    .collect::<Vec<String>>()
                    .join(", ");
                let ret = return_type
                    .as_ref()
                    .map(|r| format!(" -> {}", r))
                    .unwrap_or_default();
                let req = requires
                    .as_ref()
                    .map(|r| format!(" requires {{ {} }}", r.print_expr()))
                    .unwrap_or_default();
                let ens = ensures
                    .as_ref()
                    .map(|r| format!(" ensures {{ {} }}", r.print_expr()))
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
            CheckedAst::TypeDecl { name, .. } => {
                format!("type {}", name)
            }
        }
    }

    pub fn pretty_print(&self) -> String {
        match self {
            Self::Literal { value: l, .. } => l.pretty_print(),
            Self::Tuple { exprs: t, .. } => {
                let mut result = "(".to_string();
                for (i, v) in t.iter().enumerate() {
                    result.push_str(&v.pretty_print());
                    if i < t.len() - 1 {
                        result.push_str(", ");
                    }
                }
                result.push(')');
                result
            }
            Self::List { exprs: l, .. } => {
                let mut result = "[".to_string();
                for (i, v) in l.iter().enumerate() {
                    result.push_str(&v.pretty_print());
                    if i < l.len() - 1 {
                        result.push_str(", ");
                    }
                }
                result.push(']');
                result
            }
            Self::Record { fields: r, .. } => {
                let mut result = "{ ".to_string();
                for (i, (k, v)) in r.iter().enumerate() {
                    result.push_str(&format!("{}: {}", k, v.pretty_print()));
                    if i < r.len() - 1 {
                        result.push_str(", ");
                    }
                }
                result.push_str(" }");
                result
            }
            Self::FieldAccess {
                expr: e, field: f, ..
            } => {
                format!("{}.{}", e.pretty_print(), f)
            }
            Self::Identifier { name, .. } => name.clone(),
            Self::FunctionCall {
                expr: function,
                arg,
                ..
            } => {
                format!("{}({})", function.pretty_print(), arg.pretty_print())
            }
            Self::Lambda { param, body, .. } => {
                format!(
                    "{} -> {}",
                    param.pattern.pretty_print(),
                    body.pretty_print()
                )
            }
            Self::Let {
                target: lhs,
                expr: rhs,
                ..
            } => {
                format!("{} = {}", lhs.pretty_print(), rhs.pretty_print())
            }
            Self::Block {
                exprs: expressions,
                ty: _,
                ..
            } => {
                let mut result = "{".to_string();
                for (i, e) in expressions.iter().enumerate() {
                    result.push_str(&format!("    {}", e.pretty_print()));
                    if i < expressions.len() - 1 {
                        result.push_str("; ");
                    }
                }
                result.push('}');
                result
            }
            Self::FunctionDecl { name, sig_type, .. } => {
                format!("fn {} :: {}", name, sig_type)
            }
            Self::FunctionDef {
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
                    .map(|p| p.pattern.pretty_print())
                    .collect::<Vec<String>>()
                    .join(", ");
                let ret = return_type
                    .as_ref()
                    .map(|r| format!(" -> {}", r))
                    .unwrap_or_default();
                let req = requires
                    .as_ref()
                    .map(|r| format!(" requires {{ {} }}", r.pretty_print()))
                    .unwrap_or_default();
                let ens = ensures
                    .as_ref()
                    .map(|r| format!(" ensures {{ {} }}", r.pretty_print()))
                    .unwrap_or_default();
                format!(
                    "fn {}({}){}{}{} = {}",
                    name,
                    params_str,
                    ret,
                    req,
                    ens,
                    body.pretty_print()
                )
            }
            Self::TypeDecl { name, .. } => {
                format!("type {}", name)
            }
        }
    }
}
