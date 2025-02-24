use crate::{
    interpreter::value::{Function, RecordKey, Value},
    lexer::token::LineInfo,
    type_checker::types::Type,
};

use super::types::{FunctionType, GetType, TypeJudgements, TypeTrait};

#[derive(Debug, Clone)]
pub struct CheckedOperator {
    pub name: String,
    pub symbol: String,
    pub handler: Function,
}

#[derive(Debug, Clone)]
pub struct CheckedParam {
    pub name: String,
    pub ty: Type,
}

impl CheckedParam {
    pub fn new(name: String, ty: Type) -> CheckedParam {
        CheckedParam { name, ty }
    }

    pub fn from_str<S: Into<String>>(name: S, ty: Type) -> CheckedParam {
        CheckedParam::new(name.into(), ty)
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
        expr_types: Type,
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
        return_type: Type,
        info: LineInfo,
    },
    /// A function definition is a named function with a list of parameters and a body expression
    FunctionDef {
        param: CheckedParam,
        body: Box<CheckedAst>,
        return_type: Type,
        ty: Type,
        info: LineInfo,
    },
    /// An assignment expression assigns a value to a variable via a matching pattern (identifier, destructuring of a tuple, record, etc.).
    Assignment {
        target: Box<CheckedAst>,
        expr: Box<CheckedAst>,
        ty: Type,
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
}

impl GetType for CheckedAst {
    fn get_type(&self) -> &Type {
        match self {
            CheckedAst::Literal { value: v, info: _ } => v.get_type(),
            CheckedAst::Tuple { expr_types: ty, .. } => ty,
            CheckedAst::List { ty, .. } => ty,
            CheckedAst::Record { ty, .. } => ty,
            CheckedAst::Identifier { ty, .. } => ty,
            CheckedAst::FunctionCall { return_type, .. } => return_type,
            CheckedAst::FunctionDef { ty, .. } => ty,
            CheckedAst::Assignment {
                target: _,
                expr: _,
                ty,
                info: _,
            } => ty,
            CheckedAst::Block {
                exprs: _,
                ty,
                info: _,
            } => ty,
        }
    }
}

impl CheckedAst {
    pub fn function_def(
        param: CheckedParam,
        body: CheckedAst,
        return_type: Type,
        info: LineInfo,
    ) -> CheckedAst {
        CheckedAst::FunctionDef {
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
            CheckedAst::Literal { value: _, info } => info,
            CheckedAst::Tuple {
                exprs: _,
                expr_types: _,
                info,
            } => info,
            CheckedAst::List {
                exprs: _,
                ty: _,
                info,
            } => info,
            CheckedAst::Record {
                fields: _,
                ty: _,
                info,
            } => info,
            CheckedAst::Identifier {
                name: _,
                ty: _,
                info,
            } => info,
            CheckedAst::FunctionCall { info, .. } => info,
            CheckedAst::FunctionDef { info, .. } => info,
            CheckedAst::Assignment {
                target: _,
                expr: _,
                ty: _,
                info,
            } => info,
            CheckedAst::Block {
                exprs: _,
                ty: _,
                info,
            } => info,
        }
    }

    pub fn specialize(&mut self, judgements: &TypeJudgements, changed: &mut bool) {
        match self {
            CheckedAst::Literal { value: _, info: _ } => (),
            CheckedAst::Tuple {
                exprs: elements,
                expr_types: ty,
                info: _,
            } => {
                for element in elements {
                    element.specialize(judgements, changed);
                }
                *ty = ty.specialize(judgements, changed);
            }
            CheckedAst::List {
                exprs: elements,
                ty,
                info: _,
            } => {
                for element in elements {
                    element.specialize(judgements, changed);
                }
                *ty = ty.specialize(judgements, changed);
            }
            CheckedAst::Record {
                fields: elements,
                ty,
                info: _,
            } => {
                for (_, element) in elements {
                    element.specialize(judgements, changed);
                }
                *ty = ty.specialize(judgements, changed);
            }
            CheckedAst::Identifier {
                name: _,
                ty,
                info: _,
            } => {
                *ty = ty.specialize(judgements, changed);
            }
            CheckedAst::FunctionCall {
                expr: function,
                arg,
                return_type,
                ..
            } => {
                function.specialize(judgements, changed);
                arg.specialize(judgements, changed);
                *return_type = return_type.specialize(judgements, changed);
            }
            CheckedAst::FunctionDef {
                info: _,
                param,
                body,
                return_type,
                ty,
            } => {
                param.ty = param.ty.specialize(judgements, changed);
                *return_type = return_type.specialize(judgements, changed);
                *ty = ty.specialize(judgements, changed);
                body.specialize(judgements, changed);
            }
            CheckedAst::Assignment {
                target: lhs,
                expr: rhs,
                ty,
                info: _,
            } => {
                lhs.specialize(judgements, changed);
                rhs.specialize(judgements, changed);
                *ty = ty.specialize(judgements, changed);
            }
            CheckedAst::Block {
                exprs: expressions,
                ty,
                info: _,
            } => {
                for expression in expressions {
                    expression.specialize(judgements, changed);
                }
                *ty = ty.specialize(judgements, changed);
            }
        }
    }

    pub fn print_sexpr(&self) -> String {
        match self {
            CheckedAst::Literal { value, info: _ } => value.pretty_print(),
            CheckedAst::Tuple {
                exprs: elements,
                expr_types: _,
                info: _,
            } => format!(
                "({})",
                elements
                    .iter()
                    .map(|e| e.print_sexpr())
                    .collect::<Vec<String>>()
                    .join(" ")
            ),
            CheckedAst::List {
                exprs: elements,
                ty: _,
                info: _,
            } => format!(
                "[{}]",
                elements
                    .iter()
                    .map(|e| e.print_sexpr())
                    .collect::<Vec<String>>()
                    .join(" ")
            ),
            CheckedAst::Record {
                fields: _elements,
                ty: _,
                info: _,
            } => todo!(),
            CheckedAst::Identifier {
                name,
                ty: _,
                info: _,
            } => name.clone(),
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
                    function.print_sexpr(),
                    args.iter()
                        .map(|a| a.print_sexpr())
                        .collect::<Vec<String>>()
                        .join(", ")
                )
            }
            CheckedAst::FunctionDef { param, body, .. } => {
                format!("({} {} -> {})", param.ty, param.name, body.print_sexpr())
            }
            CheckedAst::Assignment {
                target: lhs,
                expr: rhs,
                ty: _,
                info: _,
            } => {
                format!("({} = {})", lhs.print_sexpr(), rhs.print_sexpr())
            }
            CheckedAst::Block {
                exprs: expressions,
                ty: _,
                info: _,
            } => format!(
                "{{{}}}",
                expressions
                    .iter()
                    .map(|e| e.print_sexpr())
                    .collect::<Vec<String>>()
                    .join(" ")
            ),
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
            Self::Identifier { name, .. } => name.clone(),
            Self::FunctionCall {
                expr: function,
                arg,
                ..
            } => {
                format!("{}({})", function.pretty_print(), arg.pretty_print())
            }
            Self::FunctionDef { param, body, .. } => {
                format!("{} -> {}", param.name, body.pretty_print())
            }
            Self::Assignment {
                target: lhs,
                expr: rhs,
                ty: _,
                info: _,
            } => {
                format!("{} = {}", lhs.pretty_print(), rhs.pretty_print())
            }
            Self::Block {
                exprs: expressions,
                ty: _,
                info: _,
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
        }
    }
}

/// Module is the root program node of the AST
/// It contains a list of all the expressions in the program
#[derive(Debug, Clone)]
pub struct CheckedModule {
    pub name: String,
    pub expressions: Vec<CheckedAst>,
}

impl CheckedModule {
    pub fn new(name: String, expressions: Vec<CheckedAst>) -> CheckedModule {
        CheckedModule { name, expressions }
    }
}
