use crate::{interpreter::value::RecordKey, util::str::Str};
use colorful::Colorful;
use std::fmt::{Debug, Display};

use super::checked_ast::CheckedParam;

//--------------------------------------------------------------------------------------//
//                                     Value trait                                      //
//--------------------------------------------------------------------------------------//

pub trait GetType {
    fn get_type(&self) -> &Type;
}

//--------------------------------------------------------------------------------------//
//                                     Type System                                      //
//--------------------------------------------------------------------------------------//

/// Generalized trait for type implementations
pub trait TypeTrait {
    /// Check if the type is equal to the other type.
    /// Two types are equal if they are the same type.
    /// The equality relation is reflexive, symmetric, and transitive.
    fn equals(&self, other: &Self) -> bool {
        self.subtype(other) && other.subtype(self)
    }

    /// Check if the type is a subtype of the other type.
    /// A type is a subtype of another type if it can be used in place of the other type.
    ///
    /// ## Mathematical Notation
    /// ```ignore
    /// A <: B, where A and B are types.
    /// ```
    ///
    /// ## Examples
    /// ```ignore
    /// int <: number
    /// list int <: list number
    /// list int <: list any <: any
    ///
    /// // Covariant return type and contravariant parameter type
    /// f(x: int) -> int <: f(x: number) -> number
    /// ```
    ///
    /// **Covariant return type**: The return type of the subtype function (int) is a subtype of the return type of the supertype function (number). \
    /// **Contravariant parameter type** : The parameter type of the supertype function (number) is a supertype of the parameter type of the subtype function (int).
    fn subtype(&self, other: &Self) -> bool;
    fn simplify(self) -> Self;
}

#[derive(Clone, Debug)]
pub struct FunctionType {
    pub param: CheckedParam,
    pub ret: Type,
}

impl FunctionType {
    pub fn new(param: CheckedParam, ret: Type) -> Self {
        FunctionType { param, ret }
    }

    pub fn pretty_print(&self) -> String {
        format!(
            "{} -> {}",
            self.param.ty.pretty_print(),
            self.ret.pretty_print()
        )
    }

    pub fn pretty_print_color(&self) -> String {
        format!(
            "{} {} {}",
            self.param.ty.pretty_print_color(),
            "->".dark_gray(),
            self.ret.pretty_print_color()
        )
    }
}

impl TypeTrait for FunctionType {
    fn subtype(&self, other: &Self) -> bool {
        self.param.ty.subtype(&other.param.ty) && self.ret.subtype(&other.ret)
    }

    fn simplify(self) -> Self {
        FunctionType {
            param: CheckedParam {
                name: self.param.name,
                ty: self.param.ty.simplify(),
            },
            ret: self.ret.simplify(),
        }
    }
}

#[derive(Clone, Debug)]
pub enum Type {
    /// A literal type (name).
    /// Examples such as `int`, `float`, `string`, `char`, `bool`, `unit`, `any`.
    /// Or `IO`, `List`, `Option`, `Result`, `Either`, `Tuple`, `Record`, `Function`, `UserDefinedType`.
    Literal(Str),

    /// A type alias.
    /// The first argument is the name of the type alias.
    /// The second argument is the type the alias refers to.
    /// A type alias is a type that refers to another type, and can be used interchangeably with the referred type.
    /// They are opaque to the type checker and are resolved to the referred type.
    /// But they can be used to give a user-friendly name to a complex type.
    Alias(Str, Box<Type>),

    /// A function type.
    /// The first argument is the list of parameter types.
    /// The second argument is the return type.
    Function(Box<FunctionType>),

    /// A tuple type.
    /// The first argument is the list of element types.
    /// ! There must be at least one element in the tuple.
    /// A tuple type is a product type.
    Tuple(Vec<Type>),

    /// A list type.
    /// The first argument is the type of the elements in the list.
    /// A list type is a product type.
    List(Box<Type>),

    /// A record type.
    /// The first argument is the list of fields in the record.
    /// A record type is a product type.
    Record(Vec<(RecordKey, Type)>),

    /// Generic type with name and parameters.
    /// The parameters are the type parameters of the generic type.
    /// The definition is the definition of the generic type with access to the type parameters.
    Generic(Str, Vec<Type>, Box<Type>),

    /// A sum type.
    /// The first argument is the list of variants in the sum type.
    /// A sum type must only refer to existing types. To create alterative types/data structures holding data, use an enum type.
    /// Examples of sum types are `int | float`, `string | char`, `bool | unit`, `Option | Result | Either` or
    /// `Dir = Left | Right | Up | Down | Forward(i32) | Backward(i32)`.
    Sum(Vec<Type>),

    /// A variant type.
    /// The first argument is a reference to the parent type holding a sum type of one or more variants.
    /// The second argument is the type of the boxed value.
    /// The third argument is the name of the boxed variant.
    /// A variant type is a product type of one or more wrapped types.
    ///
    /// ## Examples:
    /// ```fsharp
    /// type Box T = Wrapped T        // Box.Wrapped(T)
    /// type Option T = Some T | None
    /// type Result T E = Ok(T) | Err(E)
    /// type Either T U = Left T | Right U
    /// type List T = Nil | Cons(T, (List T))
    /// type Tree T = Leaf T | Node (Tree T) (Tree T)
    /// ```
    ///
    /// ## Note
    /// The variant type is a product type, but it is not a sum type.
    /// It is often used to create sum types with named variants and to create recursive data structures.
    ///
    /// ## Note on equality
    /// Two variant types are equal if they have the same name and the same number of fields with the same types.
    Variant(Box<Type>, Str, Vec<Type>),
}

impl TypeTrait for Type {
    fn subtype(&self, other: &Type) -> bool {
        let subtype = match (self, other) {
            (Type::Literal(Str::Str("any")), _) => true, // TODO: Find a way to use `std_types::ANY` here.
            (_, Type::Literal(Str::Str("any"))) => true,
            (Type::Literal(s1), Type::Literal(s2)) => *s1 == *s2,
            (Type::Alias(_, ty1), _) => ty1.subtype(other),
            (_, Type::Alias(_, ty)) => self.subtype(ty),
            (Type::Generic(s1, params1, _), Type::Generic(s2, params2, _)) => {
                s1 == s2
                    && params1.len() == params2.len()
                    && params1.iter().zip(params2).all(|(p1, p2)| p1.subtype(p2))
            }
            (Type::Function(ty1), Type::Function(ty2)) => {
                // ty1.len() == ty2.len() && ty1.iter().zip(ty2).all(|(t1, t2)| t1.subtype(t2))
                ty1.subtype(ty2)
            }
            (Type::Tuple(types1), Type::Tuple(types2)) => {
                types1.len() == types2.len()
                    && types1.iter().zip(types2).all(|(t1, t2)| t1.subtype(t2))
            }
            (Type::List(t1), Type::List(t2)) => t1.subtype(t2),
            (Type::Record(fields1), Type::Record(fields2)) => {
                fields1.len() == fields2.len()
                    && fields1
                        .iter()
                        .zip(fields2)
                        .all(|((n1, t1), (n2, t2))| n1 == n2 && t1.subtype(t2))
            }
            (Type::Sum(types1), Type::Sum(types2)) => {
                let mut subtype = true;
                for t1 in types1 {
                    if !types2.iter().any(|t2| t1.subtype(t2)) {
                        subtype = false;
                        break;
                    }
                }
                subtype
            }
            (_, Type::Sum(types)) => types.iter().any(|t| self.subtype(t)),
            (Type::Variant(parent1, name1, fields1), Type::Variant(parent2, name2, fields2)) => {
                parent1.equals(parent2)
                    && name1 == name2
                    && fields1.len() == fields2.len()
                    && fields1.iter().zip(fields2).all(|(t1, t2)| t1.subtype(t2))
            }
            _ => false,
        };
        subtype
    }

    fn simplify(self) -> Self {
        match self {
            Type::Literal(_) => self,
            Type::Alias(name, ty) => Type::Alias(name, Box::new(ty.simplify())),
            Type::Generic(s, params, body) => Type::Generic(
                s,
                params.into_iter().map(Type::simplify).collect(),
                Box::new(body.simplify()),
            ),
            Type::Function(ty) => {
                // Type::Function(ty.into_iter().map(FunctionType::simplify).collect())
                Type::Function(Box::new(ty.simplify()))
            }
            Type::Tuple(types) => Type::Tuple(types.into_iter().map(Type::simplify).collect()),
            Type::List(t) => Type::List(Box::new(t.simplify())),
            Type::Record(fields) => {
                Type::Record(fields.into_iter().map(|(n, t)| (n, t.simplify())).collect())
            }
            Type::Sum(types) => {
                // Flatten nested sums.
                let mut result = vec![];
                for t in types {
                    match t.simplify() {
                        Type::Sum(types) => result.extend(types),
                        t => result.push(t),
                    }
                }
                Type::Sum(result)
            }
            Type::Variant(parent, name, fields) => Type::Variant(
                parent,
                name,
                fields.into_iter().map(Type::simplify).collect(),
            ),
        }
    }
}

impl Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.clone().simplify() {
            Type::Literal(t) => write!(f, "{}", t),
            Type::Alias(name, _) => write!(f, "{}", name),
            Type::Function(func) => {
                write!(f, "{}", func.param.ty)?;
                write!(f, " -> {}", func.ret)
            }
            Type::Tuple(types) => {
                write!(f, "(")?;
                for (i, t) in types.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", t)?;
                }
                write!(f, ")")
            }
            Type::List(t) => write!(f, "[{}]", t),
            Type::Record(fields) => {
                write!(f, "{{")?;
                for (i, (name, t)) in fields.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}: {}", name, t)?;
                }
                write!(f, "}}")
            }
            Type::Sum(types) => {
                write!(f, "(")?;
                for (i, t) in types.iter().enumerate() {
                    if i > 0 {
                        write!(f, " | ")?;
                    }
                    write!(f, "{}", t)?;
                }
                write!(f, ")")
            }
            Type::Generic(s, params, _) => {
                write!(f, "{}<", s)?;
                if !params.is_empty() {
                    for (i, param) in params.iter().enumerate() {
                        if i > 0 {
                            write!(f, ", ")?;
                        }
                        write!(f, "{}", param)?;
                    }
                }
                write!(f, ">")
            }
            Type::Variant(_, name, fields) => {
                write!(f, "{}(", name)?;
                for (i, t) in fields.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", t)?;
                }
                write!(f, ")")
            }
        }
    }
}

impl Type {
    pub fn pretty_print(&self) -> String {
        match self.clone().simplify() {
            Type::Literal(t) => t.to_string(),
            Type::Alias(name, _) => name.to_string(),
            Type::Function(f) => f.pretty_print(),
            Type::Tuple(types) => {
                if types.is_empty() {
                    "()".to_string()
                } else {
                    let mut result = String::new();
                    result.push_str("(");
                    for (i, t) in types.iter().enumerate() {
                        if i > 0 {
                            result.push_str(", ");
                        }
                        result.push_str(&t.pretty_print());
                    }
                    result.push_str(")");
                    result
                }
            }
            Type::List(t) => format!("[{}]", t.pretty_print()),
            Type::Record(fields) => {
                let mut result = String::new();
                result.push_str("{");
                for (i, (name, t)) in fields.iter().enumerate() {
                    if i > 0 {
                        result.push_str(", ");
                    }
                    result.push_str(&format!("{}: {}", name, t.pretty_print()));
                }
                result.push_str("}");
                result
            }
            Type::Sum(types) => {
                if types.is_empty() {
                    "()".to_string()
                } else {
                    let mut result = String::new();
                    result.push_str("(");
                    for (i, t) in types.iter().enumerate() {
                        if i > 0 {
                            result.push_str(" | ");
                        }
                        result.push_str(&t.pretty_print());
                    }
                    result.push_str(")");
                    result
                }
            }
            Type::Generic(s, params, _) => {
                let mut result = String::new();
                result.push_str(&s.to_string());
                if !params.is_empty() {
                    result.push_str("<");
                    for (i, param) in params.iter().enumerate() {
                        if i > 0 {
                            result.push_str(", ");
                        }
                        result.push_str(&param.pretty_print());
                    }
                    result.push_str(">");
                }
                result
            }
            Type::Variant(_, name, fields) => {
                let mut result = String::new();
                result.push_str(&name.to_string());
                result.push_str("(");
                for (i, t) in fields.iter().enumerate() {
                    if i > 0 {
                        result.push_str(", ");
                    }
                    result.push_str(&t.pretty_print());
                }
                result.push_str(")");
                result
            }
        }
    }

    pub fn pretty_print_color(&self) -> String {
        match self.clone().simplify() {
            Type::Literal(t) => t.to_string().light_blue().to_string(),
            Type::Alias(name, _) => name.to_string().light_blue().to_string(),
            Type::Function(f) => f.pretty_print_color(),
            Type::Tuple(types) => {
                if types.is_empty() {
                    "()".to_string()
                } else {
                    let mut result = String::new();
                    result.push_str(&"(".dark_gray().to_string());
                    for (i, t) in types.iter().enumerate() {
                        if i > 0 {
                            result.push_str(&", ".dark_gray().to_string());
                        }
                        result.push_str(&t.pretty_print_color());
                    }
                    result.push_str(&")".dark_gray().to_string());
                    result
                }
            }
            Type::List(t) => format!("[{}]", t.pretty_print_color()),
            Type::Record(fields) => {
                let mut result = String::new();
                result.push_str(&"{".dark_gray().to_string());
                for (i, (name, t)) in fields.iter().enumerate() {
                    if i > 0 {
                        result.push_str(&", ".dark_gray().to_string());
                    }
                    result.push_str(&format!("{}: {}", name, t.pretty_print_color()));
                }
                result.push_str(&"}".dark_gray().to_string());
                result
            }
            Type::Sum(types) => {
                if types.is_empty() {
                    "()".to_string()
                } else {
                    let mut result = String::new();
                    result.push_str(&"(".dark_gray().to_string());
                    for (i, t) in types.iter().enumerate() {
                        if i > 0 {
                            result.push_str(&" | ".dark_gray().to_string());
                        }
                        result.push_str(&t.pretty_print_color());
                    }
                    result.push_str(&")".dark_gray().to_string());
                    result
                }
            }
            Type::Generic(s, params, _) => {
                let mut result = String::new();
                result.push_str(&s.to_string().light_blue().to_string());
                if !params.is_empty() {
                    result.push_str(&"<".dark_gray().to_string());
                    for (i, param) in params.iter().enumerate() {
                        if i > 0 {
                            result.push_str(&", ".dark_gray().to_string());
                        }
                        result.push_str(&param.pretty_print_color());
                    }
                    result.push_str(&">".dark_gray().to_string());
                }
                result
            }
            Type::Variant(_, name, fields) => {
                let mut result = String::new();
                result.push_str(&name.to_string().light_cyan().to_string());
                result.push_str(&"(".dark_gray().to_string());
                for (i, t) in fields.iter().enumerate() {
                    if i > 0 {
                        result.push_str(&", ".dark_gray().to_string());
                    }
                    result.push_str(&t.pretty_print_color());
                }
                result.push_str(&")".dark_gray().to_string());
                result
            }
        }
    }
}

// Standard types
pub mod std_types {
    use super::*;

    /// The any type.
    /// The type of the `any` value.
    /// This is the top type.
    /// ! Should only accessible within the compiler and native functions in the standard library.
    pub const ANY: Type = Type::Literal(Str::Str("any"));

    /// The top type, supertype of all types and itself.
    pub const TYPE: Type = Type::Literal(Str::Str("type"));

    /// The unit type.
    pub const UNIT: Type = Type::Literal(Str::Str("unit"));

    /// A string type. (supports unicode)
    pub const STRING: Type = Type::Literal(Str::Str("str"));

    /// A character type. (supports unicode)
    pub const CHAR: Type = Type::Literal(Str::Str("char"));

    /// A boolean type.
    pub const BOOL: Type = Type::Literal(Str::Str("bool"));

    //--------------------------------------------------------------------------------------//
    //                                Unsigned integer types                                //
    //--------------------------------------------------------------------------------------//

    /// A 1-bit binary value (or boolean).
    pub const UINT1: Type = Type::Literal(Str::Str("u1"));

    /// A 8-bit (1 byte) unsigned integer.
    pub const UINT8: Type = Type::Literal(Str::Str("u8"));

    /// A 16-bit (2 bytes) unsigned integer.
    pub const UINT16: Type = Type::Literal(Str::Str("u16"));

    /// A 32-bit (4 byte) unsigned integer.
    pub const UINT32: Type = Type::Literal(Str::Str("u32"));

    /// A 64-bit (8 byte) unsigned integer.
    pub const UINT64: Type = Type::Literal(Str::Str("u64"));

    /// A 128-bit (16 byte) unsigned integer.
    pub const UINT128: Type = Type::Literal(Str::Str("u128"));

    /// An arbitrary-precision unsigned integer.
    pub const UINTBIG: Type = Type::Literal(Str::Str("ubig"));

    //--------------------------------------------------------------------------------------//
    //                                 Signed integer types                                 //
    //--------------------------------------------------------------------------------------//

    /// A 8-bit (1 byte) signed integer.
    pub const INT8: Type = Type::Literal(Str::Str("i8"));

    /// A 16-bit (2 bytes) signed integer.
    pub const INT16: Type = Type::Literal(Str::Str("i16"));

    /// A 32-bit (4 byte) signed integer.
    pub const INT32: Type = Type::Literal(Str::Str("i32"));

    /// A 64-bit (8 byte) signed integer.
    pub const INT64: Type = Type::Literal(Str::Str("i64"));

    /// A 128-bit (16 byte) signed integer.
    pub const INT128: Type = Type::Literal(Str::Str("i128"));

    /// An arbitrary-precision signed integer.
    pub const INTBIG: Type = Type::Literal(Str::Str("ibig"));

    //--------------------------------------------------------------------------------------//
    //                             Signed floating-point types                              //
    //--------------------------------------------------------------------------------------//

    /// A 32-bit (4 byte) signed floating-point number.
    pub const FLOAT32: Type = Type::Literal(Str::Str("f32"));

    /// A 64-bit (8 byte) signed floating-point number.
    pub const FLOAT64: Type = Type::Literal(Str::Str("f64"));

    /// Increases the precision of a floating-point number.
    pub const FLOATBIG: Type = Type::Literal(Str::Str("fbig"));

    //--------------------------------------------------------------------------------------//
    //   								Compound types                                      //
    //--------------------------------------------------------------------------------------//

    #[allow(non_snake_case)]
    pub fn UINT() -> Type {
        Type::Alias(
            Str::Str("uint"),
            Box::new(Type::Sum(vec![
                UINT1, UINT8, UINT16, UINT32, UINT64, UINT128, UINTBIG,
            ])),
        )
    }

    #[allow(non_snake_case)]
    pub fn INT() -> Type {
        Type::Alias(
            Str::Str("int"),
            Box::new(Type::Sum(vec![
                UINT1, UINT8, INT8, UINT16, INT16, UINT32, INT32, UINT64, INT64, UINT128, INT128,
                UINTBIG, INTBIG,
            ])),
        )
    }

    #[allow(non_snake_case)]
    pub fn FLOAT() -> Type {
        Type::Alias(
            Str::Str("float"),
            Box::new(Type::Sum(vec![FLOAT32, FLOAT64, FLOATBIG])),
        )
    }

    /// A number type.
    /// The type of all numbers, both integers and floating-point numbers.
    #[allow(non_snake_case)]
    pub fn NUM() -> Type {
        Type::Alias(Str::Str("num"), Box::new(Type::Sum(vec![INT(), FLOAT()])))
    }
}
