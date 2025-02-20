use std::{borrow::Borrow, fmt::Display};

use crate::type_checker::{
    checked_ast::{CheckedAst, CheckedParam},
    types::{std_types, FunctionType, GetType, Type, TypeTrait},
};

use super::{environment::Environment, interpreter::InterpretResult, number::Number};

/// A key in a record can be a string, integer, float, or character.
/// This is used to represent the key in the AST.
///
/// ## Example
/// ```ignore
/// record = { "key": 1, 2: 3.0, 'c': "value", 4.0: 'd' }
/// ```
#[derive(Debug, Clone, PartialEq)]
pub enum RecordKey {
    String(String),
    Number(Number),
    Char(char),
}

impl Display for RecordKey {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            RecordKey::String(s) => write!(f, "{}", s),
            RecordKey::Number(n) => write!(f, "{}", n),
            RecordKey::Char(c) => write!(f, "{}", c),
        }
    }
}

/// User-defined functions
#[derive(Debug, Clone)]
pub struct UserFunction {
    pub param: CheckedParam,
    pub body: CheckedAst,
    pub closure: Environment<'static>,
    pub ret: Type,
}

/// A function handler takes a list of arguments and returns a result.
pub type NativeHandler = fn(Vec<Value>) -> InterpretResult;

/// Built-in functions
#[derive(Debug, Clone)]
pub struct NativeFunction {
    /// Name of the function
    pub name: String,
    /// The function handler
    pub handler: NativeHandler,
    /// The function parameters
    pub params: Vec<CheckedParam>,
    /// The return type of the function
    pub ret: Type,
}

#[derive(Debug, Clone)]
pub enum Function {
    User(UserFunction),
    Native(NativeFunction),
}

impl Function {
    pub fn new_user(
        param: CheckedParam,
        body: CheckedAst,
        closure: Environment<'static>,
        ret: Type,
    ) -> Self {
        Self::User(UserFunction {
            param,
            body,
            closure,
            ret,
        })
    }

    pub fn new_native(
        name: String,
        handler: NativeHandler,
        params: Vec<CheckedParam>,
        ret: Type,
    ) -> Self {
        Self::Native(NativeFunction {
            name,
            handler,
            params,
            ret,
        })
    }

    pub fn get_return_type(&self) -> &Type {
        match self {
            Function::User(UserFunction { ret, .. }) => ret,
            Function::Native(NativeFunction { ret, .. }) => ret,
        }
    }

    pub fn get_type(&self) -> FunctionType {
        match self {
            Function::User(UserFunction {
                param: params, ret, ..
            }) => FunctionType::new(params.clone(), ret.clone()),
            Function::Native(NativeFunction { params, ret, .. }) => {
                let mut params = params.iter();
                let mut function_type =
                    FunctionType::new(params.next().unwrap().clone(), ret.clone());
                for param in params {
                    function_type = FunctionType::new(
                        param.clone(),
                        Type::Function(Box::new(function_type.clone())),
                    );
                }
                function_type
            }
        }
    }

    pub fn pretty_print_color(&self) -> String {
        // TODO: Improve this ugly code
        Type::Function(Box::new(self.get_type().clone())).pretty_print_color()
    }
}

impl Display for Function {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        // TODO: And this ugly code too
        Type::Function(Box::new(self.get_type().clone())).fmt(f)
    }
}

/// A Lento value is a value that can be stored in a variable, returned from a function, or passed as an argument.
/// These values are stored in the interpreter's memory during runtime and are garbage collected when they are no longer in use.
/// The interpreter takes AST nodes and evaluates them to produce a value.
/// Values can be referenced and used in other expressions.
#[derive(Debug, Clone)]
pub enum Value {
    Unit,
    Number(Number),
    String(String),
    Char(char),
    Boolean(bool),
    Tuple(Vec<Value>, Type),
    List(Vec<Value>, Type),
    Record(Vec<(RecordKey, Value)>, Type),
    Function(Box<Function>),
    Type(Type),
}

impl GetType for Value {
    fn get_type(&self) -> &Type {
        match self {
            Value::Unit => &std_types::UNIT,
            Value::Number(n) => n.get_type(),
            Value::String(_) => &std_types::STRING,
            Value::Char(_) => &std_types::CHAR,
            Value::Boolean(_) => &std_types::BOOL,
            Value::Tuple(_, t) => t,
            Value::List(_, t) => t,
            Value::Record(_, t) => t,
            Value::Function(f) => f.get_return_type(),
            Value::Type(_) => &std_types::TYPE,
        }
    }
}

impl Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Unit => write!(f, "()"),
            Value::Number(n) => n.fmt(f),
            Value::String(s) => write!(f, "{}", s),
            Value::Char(c) => write!(f, "{}", c),
            Value::Boolean(b) => write!(f, "{}", b),
            Value::Tuple(t, _) => {
                write!(f, "(")?;
                for (i, v) in t.iter().enumerate() {
                    write!(f, "{}", v)?;
                    if i < t.len() - 1 {
                        write!(f, ", ")?;
                    }
                }
                write!(f, ")")
            }
            Value::List(l, _) => {
                write!(f, "[")?;
                for (i, v) in l.iter().enumerate() {
                    write!(f, "{}", v)?;
                    if i < l.len() - 1 {
                        write!(f, ", ")?;
                    }
                }
                write!(f, "]")
            }
            Value::Record(r, _) => {
                write!(f, "{{ ")?;
                for (i, (k, v)) in r.iter().enumerate() {
                    write!(f, "{}: {}", k, v)?;
                    if i < r.len() - 1 {
                        write!(f, ", ")?;
                    }
                }
                write!(f, " }}")
            }
            Value::Function(fun) => match fun.borrow() {
                Function::User(UserFunction {
                    param,
                    body: _,
                    closure: _,
                    ret,
                }) => {
                    writeln!(f, "{} -> {}", param.ty, ret)
                }
                Function::Native(NativeFunction { params, ret, .. }) => {
                    write!(f, "(")?;
                    for (i, p) in params.iter().enumerate() {
                        write!(f, "{}", p.ty)?;
                        if i < params.len() - 1 {
                            write!(f, ", ")?;
                        }
                    }
                    write!(f, ") -> {}", ret)
                }
            },
            Value::Type(ty) => write!(f, "{}", ty),
        }
    }
}

impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Unit, Self::Unit) => true,
            (Self::Number(l0), Self::Number(r0)) => l0 == r0,
            (Self::String(l0), Self::String(r0)) => l0 == r0,
            (Self::Char(l0), Self::Char(r0)) => l0 == r0,
            (Self::Boolean(l0), Self::Boolean(r0)) => l0 == r0,
            (Self::Tuple(l0, _), Self::Tuple(r0, _)) => l0 == r0,
            (Self::List(l0, _), Self::List(r0, _)) => l0 == r0,
            (Self::Record(l0, _), Self::Record(r0, _)) => l0 == r0,
            (Self::Function(l0), Self::Function(r0)) => l0.get_type().equals(&r0.get_type()),
            (Self::Type(l0), Self::Type(r0)) => l0.equals(r0),
            _ => false,
        }
    }
}

impl Value {
    pub fn pretty_print(&self) -> String {
        match self {
            Value::Unit => "()".to_string(),
            Value::Number(n) => n.to_string(),
            Value::String(s) => format!("\"{}\"", s),
            Value::Char(c) => format!("'{}'", c),
            Value::Boolean(b) => b.to_string(),
            Value::Tuple(t, _) => {
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
            Value::List(l, _) => {
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
            Value::Record(r, _) => {
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
            Value::Function(fun) => fun.to_string(),
            Value::Type(ty) => ty.to_string(),
        }
    }

    pub fn pretty_print_color(&self) -> String {
        use colorful::Colorful;

        match self {
            Value::Unit => format!("{}", "()".light_gray()),
            Value::Number(_) => format!("{}", self).yellow().to_string(),
            Value::String(s) => format!("\"{}\"", s).light_yellow().to_string(),
            Value::Char(c) => format!("'{}'", c).light_green().to_string(),
            Value::Boolean(b) => format!("{}", b.to_string().magenta()),
            Value::Tuple(t, _) => {
                let mut result = "(".light_gray().to_string();
                for (i, v) in t.iter().enumerate() {
                    result.push_str(&v.pretty_print_color());
                    if i < t.len() - 1 {
                        result.push_str(&", ".light_gray().to_string());
                    }
                }
                result.push_str(&")".light_gray().to_string());
                result
            }
            Value::List(l, _) => {
                let mut result = "[".light_gray().to_string();
                for (i, v) in l.iter().enumerate() {
                    result.push_str(&v.pretty_print_color());
                    if i < l.len() - 1 {
                        result.push_str(&", ".light_gray().to_string());
                    }
                }
                result.push_str(&"]".light_gray().to_string());
                result
            }
            Value::Record(r, _) => {
                let mut result = "{ ".light_gray().to_string();
                for (i, (k, v)) in r.iter().enumerate() {
                    result.push_str(&format!("{}: ", k));
                    result.push_str(&v.pretty_print_color());
                    if i < r.len() - 1 {
                        result.push_str(&", ".light_gray().to_string());
                    }
                }
                result.push_str(&" }".light_gray().to_string());
                result
            }
            Value::Function(fun) => fun.pretty_print_color(),
            Value::Type(ty) => ty.pretty_print_color(),
        }
    }
}
