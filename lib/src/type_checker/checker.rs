use super::{
    checked_ast::{ArrayLenAst, CheckedAst, CheckedParam, TypeAst},
    types::{std_types, ArrayLen, FunctionType, GetType, Type, TypeJudgeResult, TypeTrait},
};
use crate::{
    interpreter::{
        number::{Number, UnsignedInteger},
        value::{RecordKey, Value},
    },
    parser::{
        ast::Ast,
        error::ParseError,
        op::{OpHandler, OpInfo, Operator, RuntimeOpHandler, StaticOpAst, StaticOpHandler},
        pattern::BindPattern,
    },
    util::error::{BaseError, BaseErrorExt, LineInfo},
};
use colorful::Colorful;
use std::{
    borrow::Borrow,
    collections::{HashMap, HashSet},
};

/// A type error is an error that occurs during type checking.
#[derive(Debug)]
pub struct TypeError {
    inner: BaseError,
}

impl BaseErrorExt for TypeError {
    fn new(message: String, info: LineInfo) -> Self {
        Self {
            inner: BaseError::new(message, info),
        }
    }

    fn with_hint(self, hint: String) -> Self {
        Self {
            inner: self.inner.with_hint(hint),
        }
    }

    fn with_label(self, message: String, info: LineInfo) -> Self {
        Self {
            inner: self.inner.with_label(message, info),
        }
    }

    fn base(&self) -> &BaseError {
        &self.inner
    }

    fn to_base(self) -> BaseError {
        self.inner
    }
}

/// The result of the type checker stage.
/// This is a type error variant that can be either a type error or a parse error.
#[derive(Debug)]
pub enum TypeErrorVariant {
    /// A type error occurred during type checking
    TypeError(TypeError),
    /// A parse error occurred during type checking,
    /// this should only happen if a static operator handler is used.
    ParseError(ParseError),
}

impl From<TypeError> for TypeErrorVariant {
    fn from(err: TypeError) -> Self {
        Self::TypeError(err)
    }
}

impl From<ParseError> for TypeErrorVariant {
    fn from(err: ParseError) -> Self {
        Self::ParseError(err)
    }
}

// The result of the type checker stage
pub type TypeCheckerResult<T> = Result<T, TypeErrorVariant>;

/// The type environment contains all the types and functions in the program.
/// It is used to check the types of expressions and functions.
#[derive(Debug, Default, Clone)]
struct TypeEnv {
    // The variable environment
    variables: HashMap<String, Type>,

    // The function environment
    functions: HashMap<String, Vec<FunctionType>>,

    // The type environment
    types: HashMap<String, Type>,

    // Type declaration parameters by alias name
    type_params: HashMap<String, Vec<String>>,

    // The operators environment
    operators: Vec<Operator>,
}

impl TypeEnv {
    // Add a function to the type environment
    pub fn add_function(&mut self, name: String, variation: FunctionType) {
        self.functions.entry(name).or_default().push(variation);
    }

    pub fn lookup_function(&self, name: &str) -> Option<&[FunctionType]> {
        self.functions.get(name).map(Vec::as_ref)
    }

    pub fn lookup_variable(&self, name: &str) -> Option<&Type> {
        self.variables.get(name)
    }

    pub fn lookup_type(&self, name: &str) -> Option<&Type> {
        self.types.get(name)
    }

    // Add a type to the type environment
    pub fn add_type(&mut self, name: &str, ty: Type) {
        self.types.insert(name.to_string(), ty);
    }

    pub fn add_type_with_params(&mut self, name: &str, ty: Type, params: Vec<String>) {
        self.types.insert(name.to_string(), ty);
        if !params.is_empty() {
            self.type_params.insert(name.to_string(), params);
        }
    }

    pub fn lookup_type_params(&self, name: &str) -> Option<&[String]> {
        self.type_params.get(name).map(Vec::as_slice)
    }

    // Add a variable to the type environment
    pub fn add_variable(&mut self, name: String, ty: Type) {
        self.variables.insert(name, ty);
    }

    // Add an operator to the type environment
    pub fn add_operator(&mut self, op: Operator) {
        self.operators.push(op);
    }
}

enum IdentifierType<'a> {
    Variable(&'a Type),
    Type(&'a Type),
    Function(&'a [FunctionType]),
}

/// The type checker is used to check the types of expressions and functions.
#[derive(Debug, Default)]
pub struct TypeChecker<'a> {
    // The type environment
    env: TypeEnv,
    parent: Option<&'a TypeChecker<'a>>,
}

impl TypeChecker<'_> {
    // ================== Type environment functions ==================

    pub fn reset(&mut self) {
        self.env = TypeEnv::default();
    }

    pub fn add_type(&mut self, name: &str, ty: Type) {
        self.env.add_type(name, ty);
    }

    pub fn add_type_with_params(&mut self, name: &str, ty: Type, params: Vec<String>) {
        self.env.add_type_with_params(name, ty, params);
    }

    pub fn add_operator(&mut self, op: Operator) {
        self.env.add_operator(op);
    }

    pub fn add_function(&mut self, name: &str, variation: FunctionType) {
        self.env.add_function(name.to_string(), variation);
    }

    pub fn add_variable(&mut self, name: String, ty: Type) {
        self.env.add_variable(name, ty);
    }

    fn new_scope(&self) -> TypeChecker<'_> {
        TypeChecker {
            env: TypeEnv::default(),
            parent: Some(self),
        }
    }

    fn lookup_function(&self, name: &str) -> Option<&[FunctionType]> {
        self.env
            .lookup_function(name)
            .or_else(|| self.parent.and_then(|p| p.lookup_function(name)))
    }

    fn lookup_variable(&self, name: &str) -> Option<&Type> {
        self.env
            .lookup_variable(name)
            .or_else(|| self.parent.and_then(|p| p.lookup_variable(name)))
    }

    fn lookup_type(&self, name: &str) -> Option<&Type> {
        self.env
            .lookup_type(name)
            .or_else(|| self.parent.and_then(|p| p.lookup_type(name)))
    }

    fn lookup_type_params(&self, name: &str) -> Option<&[String]> {
        self.env
            .lookup_type_params(name)
            .or_else(|| self.parent.and_then(|p| p.lookup_type_params(name)))
    }

    fn lookup_identifier(&self, name: &str) -> Option<IdentifierType<'_>> {
        Some(if let Some(ty) = self.lookup_type(name) {
            IdentifierType::Type(ty)
        } else if let Some(variants) = self.lookup_function(name) {
            IdentifierType::Function(variants)
        } else if let Some(ty) = self.lookup_variable(name) {
            IdentifierType::Variable(ty)
        } else {
            return None;
        })
    }

    fn lookup_local_identifier(&self, name: &str) -> Option<IdentifierType<'_>> {
        Some(if let Some(ty) = self.env.lookup_type(name) {
            IdentifierType::Type(ty)
        } else if let Some(variants) = self.env.lookup_function(name) {
            IdentifierType::Function(variants)
        } else if let Some(ty) = self.env.lookup_variable(name) {
            IdentifierType::Variable(ty)
        } else {
            return None;
        })
    }

    fn lookup_operator(&self, symbol: &str) -> Vec<&Operator> {
        let operators: Vec<&Operator> = self
            .env
            .operators
            .iter()
            .filter(|o| o.info.symbol == symbol)
            .chain(self.parent.iter().flat_map(|p| p.lookup_operator(symbol)))
            .collect();
        operators
    }

    fn lookup_static_operator(&self, symbol: &str) -> Option<&StaticOpHandler> {
        let operator: Option<&StaticOpHandler> = self.env.operators.iter().find_map(|o| {
            if o.info.symbol == symbol {
                if let OpHandler::Static(op) = &o.handler {
                    return Some(op);
                }
            }
            None
        });
        let operator =
            operator.or_else(|| self.parent.and_then(|p| p.lookup_static_operator(symbol)));
        operator
    }

    fn select_function_variant(
        &self,
        name: &str,
        variants: &[FunctionType],
        arg: &CheckedAst,
        info: &LineInfo,
    ) -> TypeCheckerResult<(FunctionType, TypeJudgeResult)> {
        let mut closest_match = None;
        let mut matches = Vec::new();
        for variant in variants {
            let tr = arg.get_type().subtype(&variant.param.ty);
            if tr.success {
                matches.push((variant, tr));
            }
            closest_match = Some(variant);
        }
        if let Some((variant, tr)) = matches.iter().find(|(candidate, _)| {
            matches.iter().all(|(other, _)| {
                candidate.param.ty.subtype(&other.param.ty).success
                    || !other.param.ty.subtype(&candidate.param.ty).success
            })
        }) {
            return Ok(((*variant).clone(), tr.clone()));
        }

        let mut err = TypeError::new(
            format!(
                "No variant of {} accepts {}",
                name.yellow(),
                arg.get_type().pretty_print_color()
            ),
            info.clone(),
        )
        .with_label(
            format!("This is of type {}", arg.get_type().pretty_print_color()),
            arg.info().clone(),
        );
        if let Some(closest_match) = closest_match {
            err = err.with_hint(format!(
                "Closest variant expects {}",
                closest_match.param.ty.pretty_print_color()
            ));
        }
        Err(err.into())
    }

    fn select_operator_handler_variant(
        &self,
        name: &str,
        variants: &[FunctionType],
        signature: &crate::parser::op::OpSignature,
        info: &LineInfo,
    ) -> TypeCheckerResult<FunctionType> {
        let expected = signature.function_type();
        variants
            .iter()
            .find(|variant| variant.equals(&expected).success)
            .cloned()
            .ok_or_else(|| {
                TypeError::new(
                    format!(
                        "Handler function {} has no variant matching {}",
                        name.yellow(),
                        Type::Function(Box::new(expected)).pretty_print_color()
                    ),
                    info.clone(),
                )
                .into()
            })
    }

    fn checked_call(
        &self,
        expr: CheckedAst,
        fn_ty: FunctionType,
        arg: CheckedAst,
        tr: TypeJudgeResult,
        info: &LineInfo,
    ) -> CheckedAst {
        let mut changed = false;
        let specialized_fn_ty = fn_ty.specialize(&tr.judgements, &mut changed);
        if changed {
            log::trace!(
                "Specialized call: {} : {} -> {}",
                expr.print_expr(),
                specialized_fn_ty.param.ty.pretty_print_color(),
                specialized_fn_ty.return_type.pretty_print_color()
            );
        }

        let ret_ty = specialized_fn_ty.return_type.clone();
        let expr = match expr {
            CheckedAst::Identifier { name, info, .. } => CheckedAst::Identifier {
                name,
                ty: Type::Function(Box::new(specialized_fn_ty)),
                info,
            },
            expr => expr,
        };

        CheckedAst::FunctionCall {
            ret_ty,
            expr: Box::new(expr),
            arg: Box::new(arg),
            info: info.clone(),
        }
    }

    // ================== Scanning functions ==================

    fn scan_forward(&mut self, expr: &[Ast]) -> TypeCheckerResult<()> {
        for e in expr {
            if let Ast::FunctionDef {
                name,
                params,
                return_type,
                requires,
                ensures,
                body,
                info,
            } = e
            {
                let _ = requires;
                let _ = ensures;
                let (checked_params, declared_return) =
                    self.check_function_signature(params, return_type)?;
                let checked_body = self.check_function_body(&checked_params, body)?;
                let return_ty = declared_return.unwrap_or_else(|| checked_body.get_type().clone());

                let mut variation: Option<FunctionType> = None;
                for p in checked_params.iter().rev() {
                    variation = Some(FunctionType {
                        param: p.clone(),
                        return_type: variation
                            .map(|f| Type::Function(Box::new(f)))
                            .unwrap_or_else(|| return_ty.clone()),
                    });
                }
                let variation = variation.ok_or_else(|| {
                    TypeErrorVariant::TypeError(TypeError::new(
                        "Expected at least one function parameter".to_string(),
                        info.clone(),
                    ))
                })?;
                self.env.add_function(name.clone(), variation);
                continue;
            }
            if let Ast::FunctionDecl {
                name, signature, ..
            } = e
            {
                // Register the declared name with the declared type signature.
                let sig_type = self.check_type_expr(signature)?;
                self.env.add_variable(name.clone(), sig_type);
                continue;
            }
            if let Ast::TypeDecl {
                name, params, body, ..
            } = e
            {
                let mut type_scope = self.new_scope();
                type_scope.add_type("Self", Type::Variable(name.clone().into()));
                let mut param_names = Vec::new();
                for p in params {
                    let Ast::Identifier {
                        name: param_name, ..
                    } = p
                    else {
                        continue;
                    };
                    param_names.push(param_name.clone());
                    type_scope.add_type(param_name, Type::Variable(param_name.clone().into()));
                }
                let body_ty = type_scope.check_type_expr(body)?;
                self.env.add_type_with_params(
                    name,
                    Type::Alias(name.clone().into(), Box::new(body_ty)),
                    param_names,
                );
                continue;
            }
            if let Ast::Let { target, expr, .. } = e {
                let BindPattern::Variable { name, .. } = target else {
                    continue;
                };
                let Ast::Lambda {
                    param, body, info, ..
                } = expr.borrow()
                else {
                    continue;
                };
                let checked_param = self.check_param(param)?;
                let checked = self.check_lambda(checked_param.clone(), body, info)?;
                let return_type = if let CheckedAst::Lambda { return_type, .. } = &checked {
                    return_type.clone()
                } else {
                    checked.get_type().clone()
                };
                let variation = FunctionType {
                    param: checked_param,
                    return_type,
                };
                log::debug!(
                    "Adding function {} with variation {}",
                    name.clone().yellow(),
                    variation.pretty_print()
                );
                self.env.add_function(name.clone(), variation);
            }
        }
        Ok(())
    }

    // ================== Type checking functions ==================

    pub fn check_top_exprs(&mut self, exprs: &[Ast]) -> TypeCheckerResult<Vec<CheckedAst>> {
        self.scan_forward(exprs)?;
        let mut res = vec![];
        for e in exprs {
            res.push(self.check_expr(e)?);
        }
        Ok(res)
    }

    /// Check the type of an expression
    pub fn check_expr(&mut self, expr: &Ast) -> TypeCheckerResult<CheckedAst> {
        Ok(match expr {
            Ast::Lambda {
                param, body, info, ..
            } => self.check_lambda(self.check_param(param)?, body, info)?,
            Ast::Literal { value, info } => CheckedAst::Literal {
                value: value.clone(),
                info: info.clone(),
            },
            Ast::Tuple { exprs, info } => self.check_tuple(exprs, info)?,
            Ast::List { exprs: elems, info } => self.check_list(elems, info)?,
            Ast::Array { elem, len, info } => self.check_array(elem, len, info)?,
            Ast::Record { fields, info } => self.check_record(fields, info)?,
            Ast::MemberAccess {
                expr: record,
                field,
                info,
            } => self.check_field_access(record, field, info)?,
            Ast::Identifier { name, info } => self.check_identifier(name, info)?,
            Ast::FunctionCall { expr, arg, info } => self.check_call(expr, arg, info)?,
            Ast::Binary {
                lhs,
                op: op_info,
                rhs,
                info,
            } => self.check_binary(lhs, op_info, rhs, info)?,
            Ast::Unary {
                op,
                expr: operand,
                info,
            } => self.check_unary(op, operand, info)?,
            Ast::Let {
                target,
                expr,
                annotation,
                info,
            } => self.check_let(target, expr, annotation, info)?,
            Ast::Block { exprs, info } => self.check_block(exprs, info)?,
            Ast::FunctionDecl {
                name,
                signature,
                info,
            } => {
                let sig_type = self.check_type_expr(signature)?;
                CheckedAst::FunctionDecl {
                    name: name.clone(),
                    sig_type,
                    info: info.clone(),
                }
            }
            Ast::FunctionDef {
                name,
                params,
                return_type,
                requires,
                ensures,
                body,
                info,
            } => {
                let (checked_params, declared_return) =
                    self.check_function_signature(params, return_type)?;
                let declared_requires = if let Some(req_ast) = requires {
                    Some(Box::new(self.check_expr(req_ast)?))
                } else {
                    None
                };
                let declared_ensures = if let Some(ens_ast) = ensures {
                    Some(Box::new(self.check_expr(ens_ast)?))
                } else {
                    None
                };
                let checked_body = self.check_function_body(&checked_params, body)?;
                let ret_type = declared_return.unwrap_or_else(|| checked_body.get_type().clone());

                CheckedAst::FunctionDef {
                    name: name.clone(),
                    params: checked_params,
                    return_type: Some(ret_type),
                    requires: declared_requires,
                    ensures: declared_ensures,
                    body: Box::new(checked_body),
                    info: info.clone(),
                }
            }
            Ast::TypeDecl {
                name,
                params,
                body,
                info,
            } => {
                let mut type_scope = self.new_scope();
                type_scope.add_type("Self", Type::Variable(name.clone().into()));
                for p in params {
                    let Ast::Identifier {
                        name: param_name, ..
                    } = p
                    else {
                        continue;
                    };
                    type_scope.add_type(param_name, Type::Variable(param_name.clone().into()));
                }
                let _ = type_scope.check_type_expr(body)?;
                CheckedAst::TypeDecl {
                    name: name.clone(),
                    info: info.clone(),
                }
            }
        })
    }

    fn check_type_expr(&self, expr: &TypeAst) -> TypeCheckerResult<Type> {
        Ok(match expr {
            TypeAst::Identifier { name, info } => std_types::from_str(name)
                .or_else(|| self.lookup_type(name).cloned())
                .ok_or_else(|| {
                    TypeError::new(format!("Unknown type {}", name.clone().red()), info.clone())
                        .with_label("This type is not defined".to_string(), info.clone())
                })?,
            TypeAst::Application {
                expr,
                args: params,
                info,
            } => {
                let expr_info = expr.info();
                let Type::Alias(base_name, base_type) = self.check_type_expr(expr)? else {
                    return Err(TypeError::new(
                        format!(
                            "Cannot use constructor on non-constructor type {}",
                            expr.print_expr()
                        ),
                        info.clone(),
                    )
                    .with_label(
                        format!("This is not a constructable type {}", expr.print_expr()),
                        expr_info.clone(),
                    )
                    .into());
                };
                let args = params
                    .iter()
                    .map(|a| self.check_type_expr(a))
                    .collect::<TypeCheckerResult<Vec<_>>>()?;

                let param_names = self
                    .lookup_type_params(&base_name.to_string())
                    .unwrap_or(&[]);
                if !param_names.is_empty() {
                    if args.len() != param_names.len() {
                        return Err(TypeError::new(
                            format!(
                                "Type {} expects {} argument(s), found {}",
                                base_name,
                                param_names.len(),
                                args.len()
                            ),
                            info.clone(),
                        )
                        .with_label(
                            format!(
                                "Expected {} type argument(s) for {}",
                                param_names.len(),
                                base_name
                            ),
                            expr_info.clone(),
                        )
                        .into());
                    }
                    let judgements = param_names
                        .iter()
                        .cloned()
                        .zip(args.iter().cloned())
                        .map(|(name, ty)| (name.into(), ty))
                        .collect();
                    let mut changed = false;
                    base_type.specialize(&judgements, &mut changed)
                } else {
                    Type::Constructor(base_name, args, base_type)
                }
            }
            TypeAst::Tuple { items, .. } => {
                let elems = items
                    .iter()
                    .map(|a| self.check_type_expr(a))
                    .collect::<TypeCheckerResult<Vec<_>>>()?;
                Type::Tuple(elems)
            }
            TypeAst::Array { elem, len, .. } => {
                let elem_ty = self.check_type_expr(elem)?;
                Type::Array(
                    Box::new(elem_ty),
                    match len {
                        ArrayLenAst::Known(len) => ArrayLen::Known(*len),
                        ArrayLenAst::Symbol(name) => ArrayLen::Symbol(name.clone().into()),
                    },
                )
            }
            TypeAst::List { elem, .. } => {
                let elem_ty = self.check_type_expr(elem)?;
                Type::List(Box::new(elem_ty))
            }
            TypeAst::Record { fields, .. } => {
                let fields = fields
                    .iter()
                    .map(|(k, v)| Ok((k.clone(), self.check_type_expr(v)?)))
                    .collect::<TypeCheckerResult<Vec<_>>>()?;
                Type::Record(fields)
            }
            TypeAst::Refinement { base, .. } => self.check_type_expr(base)?,
            TypeAst::Sum { variants, .. } => {
                let variants = variants
                    .iter()
                    .map(|v| self.check_type_expr(v))
                    .collect::<TypeCheckerResult<Vec<_>>>()?;
                Type::Sum(variants).simplify()
            }
            TypeAst::Lambda { lhs, rhs, .. } => {
                let lhs_ty = self.check_type_expr(lhs)?;
                let rhs_ty = self.check_type_expr(rhs)?;
                Type::Function(Box::new(FunctionType::new(
                    CheckedParam::from_str("_", lhs_ty),
                    rhs_ty,
                )))
            }
            TypeAst::Literal { value, .. } => value.get_type().clone(),
        })
    }

    fn check_param(&self, param: &Ast) -> TypeCheckerResult<CheckedParam> {
        let pattern =
            BindPattern::from_expr(param.clone()).map_err(TypeErrorVariant::ParseError)?;
        Ok(CheckedParam {
            pattern,
            ty: std_types::ANY,
        })
    }

    fn check_lambda(
        &mut self,
        param: CheckedParam,
        body: &Ast,
        info: &LineInfo,
    ) -> TypeCheckerResult<CheckedAst> {
        let mut body_scope = self.new_scope();
        let pattern_names = binding_typed_names(&param.pattern, &param.ty);
        for (name, ty) in pattern_names.into_iter() {
            body_scope.add_variable(name, ty);
        }
        let checked_body = body_scope.check_expr(body)?;
        let body_type = checked_body.get_type().clone();
        // let return_type = if let Some(ty) = &return_type {
        //     let ty = self.check_type_expr(ty)?;
        //     if !ty.subtype(&body_type).success {
        //         return Err(TypeError::new(
        //             format!(
        //                 "Function body type does not match the return type. Expected {}, found {}",
        //                 ty.pretty_print_color(),
        //                 body_type.pretty_print_color()
        //             ),
        //             info.clone(),
        //         )
        //         .with_label(
        //             format!("This is not of type {}", ty.pretty_print_color()),
        //             body.last_info().clone(),
        //         )
        //         .into());
        //     }
        //     ty
        // } else {
        //     // Infer the return type from the body
        //     body_type
        // };
        let return_type = body_type;

        Ok(CheckedAst::lambda(
            param,
            checked_body,
            return_type,
            info.clone(),
        ))
    }

    fn check_function_signature(
        &self,
        params: &[(BindPattern, Option<TypeAst>)],
        return_type: &Option<TypeAst>,
    ) -> TypeCheckerResult<(Vec<CheckedParam>, Option<Type>)> {
        let declared_return = if let Some(ret_ast) = return_type {
            Some(self.check_type_expr(ret_ast)?)
        } else {
            None
        };

        let mut checked_params = Vec::new();
        for (pattern, ty_ast) in params.iter() {
            let ty = if let Some(ty_ast) = ty_ast {
                self.check_type_expr(ty_ast)?
            } else {
                std_types::ANY
            };
            checked_params.push(CheckedParam::new(pattern.clone(), ty));
        }

        if checked_params
            .iter()
            .all(|param| param.ty.equals(&std_types::ANY).success)
        {
            if let Some(ret) = &declared_return {
                if ret.subtype(&std_types::NUM()).success {
                    for param in &mut checked_params {
                        param.ty = ret.clone();
                    }
                }
            }
        }

        Ok((checked_params, declared_return))
    }

    fn check_function_body(
        &self,
        checked_params: &[CheckedParam],
        body: &Ast,
    ) -> TypeCheckerResult<CheckedAst> {
        let mut body_scope = self.new_scope();
        for checked_param in checked_params {
            for (name, ty) in binding_typed_names(&checked_param.pattern, &checked_param.ty) {
                body_scope.add_variable(name, ty);
            }
        }
        body_scope.check_expr(body)
    }

    fn check_tuple(&mut self, elems: &[Ast], info: &LineInfo) -> TypeCheckerResult<CheckedAst> {
        if elems.is_empty() {
            return Ok(CheckedAst::unit(info.clone()));
        }
        let checked_elems = self.check_top_exprs(elems)?;
        let elem_types = checked_elems
            .iter()
            .map(|e| e.get_type())
            .cloned()
            .collect::<Vec<_>>();
        Ok(CheckedAst::Tuple {
            exprs: checked_elems,
            ty: Type::Tuple(elem_types),
            info: info.clone(),
        })
    }

    fn check_list(&mut self, elems: &[Ast], info: &LineInfo) -> TypeCheckerResult<CheckedAst> {
        let checked_elems = self.check_top_exprs(elems)?;
        let elem_types = checked_elems
            .iter()
            .map(|e| e.get_type())
            .cloned()
            .collect::<Vec<_>>();
        // Filter out duplicate types (subtypes of existing types)
        let mut list_types = vec![];
        for ty in elem_types.iter() {
            if !elem_types.iter().any(|t| ty.subtype(t).success) {
                // Add the type if it is not a subtype of any other type
                list_types.push(ty.clone());
            }
        }
        let list_type = if list_types.len() == 1 {
            list_types[0].clone()
        } else {
            Type::Sum(list_types)
        };
        Ok(CheckedAst::List {
            exprs: checked_elems,
            ty: Type::List(Box::new(list_type)),
            info: info.clone(),
        })
    }

    fn check_array(
        &mut self,
        elem: &Ast,
        len: &Ast,
        info: &LineInfo,
    ) -> TypeCheckerResult<CheckedAst> {
        let len_checked = self.check_expr(len)?;
        let len_value = match len_checked {
            CheckedAst::Literal {
                value: Value::Number(n),
                ..
            } => n,
            _ => {
                return Err(TypeError::new(
                    "Array length must be a numeric literal".to_string(),
                    len.info().clone(),
                )
                .into())
            }
        };
        let len = number_to_usize(&len_value).ok_or_else(|| {
            TypeErrorVariant::TypeError(TypeError::new(
                "Array length must be a non-negative integer".to_string(),
                len.info().clone(),
            ))
        })?;

        let checked_elem = self.check_expr(elem)?;
        let elem_ty = checked_elem.get_type().clone();
        let exprs = vec![checked_elem; len];
        Ok(CheckedAst::List {
            exprs,
            ty: Type::List(Box::new(elem_ty)),
            info: info.clone(),
        })
    }

    fn check_record(
        &mut self,
        pairs: &[(RecordKey, Ast)],
        info: &LineInfo,
    ) -> TypeCheckerResult<CheckedAst> {
        let pairs = pairs
            .iter()
            .map(|(k, v)| Ok((k.clone(), self.check_expr(v)?)))
            .collect::<TypeCheckerResult<Vec<_>>>()?;
        let record_type = Type::Record(
            pairs
                .iter()
                .map(|(k, v)| (k.clone(), v.get_type().clone()))
                .collect(),
        );
        Ok(CheckedAst::Record {
            fields: pairs,
            ty: record_type,
            info: info.clone(),
        })
    }

    fn check_field_access(
        &mut self,
        record: &Ast,
        field: &RecordKey,
        info: &LineInfo,
    ) -> TypeCheckerResult<CheckedAst> {
        let record = self.check_expr(record)?;
        let record_ty = record.get_type();
        if let Type::Record(fields) = record_ty {
            if let Some(ty) =
                fields
                    .iter()
                    .find_map(|(k, v)| if k == field { Some(v.clone()) } else { None })
            {
                Ok(CheckedAst::FieldAccess {
                    expr: Box::new(record),
                    field: field.clone(),
                    ty,
                    info: info.clone(),
                })
            } else {
                Err(TypeError::new(
                    format!(
                        "Field {} not found in record of type {}",
                        field.to_string().yellow(),
                        record_ty.pretty_print_color()
                    ),
                    info.clone(),
                )
                .with_label(
                    format!(
                        "This record does not have the field {}",
                        field.to_string().yellow()
                    ),
                    record.info().clone(),
                )
                .into())
            }
        } else {
            Err(TypeError::new(
                format!(
                    "Cannot access field {} of non-record type {}",
                    field.to_string().yellow(),
                    record_ty.pretty_print_color()
                ),
                info.clone(),
            )
            .with_label(
                format!("This is of type {}", record_ty.pretty_print_color()),
                record.info().clone(),
            )
            .into())
        }
    }

    fn check_identifier(&self, name: &str, info: &LineInfo) -> TypeCheckerResult<CheckedAst> {
        Ok(match self.lookup_identifier(name) {
            Some(IdentifierType::Variable(ty)) => CheckedAst::Identifier {
                name: name.to_string(),
                ty: ty.clone(),
                info: info.clone(),
            },
            Some(IdentifierType::Type(ty)) => CheckedAst::Literal {
                value: Value::Type(ty.clone()),
                info: info.clone(),
            },
            // TODO: Do not select the first variant!!!
            // Instead, select the variant that matches the arguments types
            // Or infer based on the context of use etc.
            // This is a very temporary solution...
            Some(IdentifierType::Function(variants)) => {
                if variants.len() == 1 {
                    let variant = &variants[0];
                    CheckedAst::Identifier {
                        name: name.to_string(),
                        ty: Type::Function(Box::new(variant.clone())),
                        info: info.clone(),
                    }
                } else if variants.is_empty() {
                    return Err(TypeError::new(
                        format!("Function {} has no variants", name.yellow()),
                        info.clone(),
                    )
                    .into());
                } else {
                    return Err(TypeError::new(
                        format!(
                            "Function {} needs a call site to resolve its variant",
                            name.yellow()
                        ),
                        info.clone(),
                    )
                    .with_label(
                        "This overloaded function is ambiguous here".to_string(),
                        info.clone(),
                    )
                    .into());
                }
            }
            None => {
                return Err(TypeError::new(
                    format!("Unknown variable {}", name.yellow()),
                    info.clone(),
                )
                .with_label("This variable is not defined".to_string(), info.clone())
                .into());
            }
        })
    }

    fn check_let(
        &mut self,
        target: &BindPattern,
        expr: &Ast,
        annotation: &Option<TypeAst>,
        info: &LineInfo,
    ) -> TypeCheckerResult<CheckedAst> {
        match target {
            BindPattern::Variable { .. } | BindPattern::MemberAccess { .. } => {
                let name = target.binding_name().expect("binding target name");
                if self.lookup_local_identifier(&name).is_some() {
                    return Err(TypeError::new(
                        format!("{} is already defined", name.clone().yellow()),
                        info.clone(),
                    )
                    .with_label(
                        "This already exists in the current scope".to_string(),
                        target.info().clone(),
                    )
                    .into());
                }
                let expr = self.check_expr(expr)?;
                let ty = expr.get_type().clone();
                if let Some(ty_ast) = annotation {
                    let expected_ty = self.check_type_expr(ty_ast)?;
                    if !ty.subtype(&expected_ty).success {
                        return Err(TypeError::new(
                            format!(
                                "Cannot assign {} to {}",
                                ty.pretty_print_color(),
                                expected_ty.pretty_print_color()
                            ),
                            info.clone(),
                        )
                        .with_label(
                            format!("This is of type {}", ty.pretty_print_color()),
                            expr.info().clone(),
                        )
                        .with_label(
                            format!("This expected type {}", expected_ty.pretty_print_color()),
                            info.clone(),
                        )
                        .into());
                    }
                }
                self.add_variable(name.clone(), ty.clone());
                Ok(CheckedAst::Let {
                    target: target.clone(),
                    expr: Box::new(expr),
                    info: info.clone(),
                })
            }
            _ => Err(TypeErrorVariant::ParseError(ParseError::new(
                format!("Invalid let binding target: {}", target.print_expr()),
                target.info().clone(),
            ))),
        }
    }

    fn check_block(&mut self, exprs: &[Ast], info: &LineInfo) -> TypeCheckerResult<CheckedAst> {
        let mut scope = self.new_scope();
        let exprs = scope.check_top_exprs(exprs)?;
        let ty = if let Some(expr) = exprs.last() {
            expr.get_type().clone()
        } else {
            std_types::UNIT
        };
        Ok(CheckedAst::Block {
            exprs,
            ty,
            info: info.clone(),
        })
    }

    fn check_call(
        &mut self,
        expr: &Ast,
        arg: &Ast,
        info: &LineInfo,
    ) -> TypeCheckerResult<CheckedAst> {
        // TODO: Add support for multiple function variants.
        // TODO: This job should be done in the type checker so that the interpreter can just call the function.
        // TODO: Go through the expression and check if the type is a function.
        let arg = self.check_expr(arg)?;
        if let Ast::Identifier {
            name,
            info: expr_info,
        } = expr
        {
            if let Some(IdentifierType::Function(variants)) = self.lookup_identifier(name) {
                let (fn_ty, tr) = self.select_function_variant(name, variants, &arg, info)?;
                let expr = CheckedAst::Identifier {
                    name: name.clone(),
                    ty: Type::Function(Box::new(fn_ty.clone())),
                    info: expr_info.clone(),
                };
                log::info!(
                    "Function call: {} : {} -> {} with argument {} : {}",
                    expr.print_expr(),
                    fn_ty.param.ty.pretty_print_color(),
                    fn_ty.return_type.pretty_print_color(),
                    arg.pretty_print(),
                    arg.get_type().pretty_print_color()
                );
                return Ok(self.checked_call(expr, fn_ty, arg, tr, info));
            }
        }

        let expr = self.check_expr(expr)?;
        if let Type::Function(fn_ty) = expr.get_type() {
            let fn_ty = fn_ty.as_ref().clone();
            let FunctionType {
                param,
                return_type: ret,
            } = fn_ty.borrow();
            let tr = arg.get_type().subtype(&param.ty);
            if tr.success {
                log::info!(
                    "Function call: {} : {} -> {} with argument {} : {}",
                    expr.print_expr(),
                    param.ty.pretty_print_color(),
                    ret.pretty_print_color(),
                    arg.pretty_print(),
                    arg.get_type().pretty_print_color()
                );
                Ok(self.checked_call(expr, fn_ty, arg, tr, info))
            } else {
                Err(TypeError::new(
                    format!(
                        "Function of type {} cannot be called with {}",
                        expr.get_type().pretty_print_color(),
                        arg.get_type().pretty_print_color()
                    ),
                    info.clone(),
                )
                .with_label(
                    format!("This is of type {}", arg.get_type().pretty_print_color()),
                    arg.info().clone(),
                )
                .with_label(
                    format!("This expected type {}", param.ty.pretty_print_color()),
                    expr.info().clone(),
                )
                .into())
            }
        } else {
            Err(TypeError::new(
                format!("Cannot call non-function: {}", expr.get_type()),
                info.clone(),
            )
            .with_label("This is not a function".into(), info.clone())
            .into())
        }
    }

    /// Check the type of a binary expression.
    /// A binary expression results in a function variation-specific call on the form:
    ///
    /// ```ignore
    /// operator : lhs -> rhs -> ret
    /// operator = [lhs, rhs] => ret
    /// // IDX:      0    1
    /// lhs `operator` rhs
    /// <==>
    /// (call
    ///     (call (identifier operator) lhs)
    ///     rhs)
    /// ```
    fn check_binary(
        &mut self,
        lhs: &Ast,
        op_info: &OpInfo,
        rhs: &Ast,
        info: &LineInfo,
    ) -> TypeCheckerResult<CheckedAst> {
        if let Some(op) = self.lookup_static_operator(&op_info.symbol) {
            log::trace!("Found static operator: {}", op_info.symbol);
            return self.check_expr(&(op.handler)(StaticOpAst::Infix(lhs.clone(), rhs.clone()))?);
        }
        let checked_lhs = self.check_expr(lhs)?;
        let checked_rhs = self.check_expr(rhs)?;
        let lhs_type = checked_lhs.get_type();
        let rhs_type = checked_rhs.get_type();
        let mut closest_match = None;
        let matching_ops = self
            .lookup_operator(&op_info.symbol)
            .into_iter()
            .filter(|op| {
                let sig = op.signature();
                if !lhs_type.subtype(&sig.params[0].ty).success {
                    log::trace!(
                        "Skipping operator: {} because lhs type {} is not a subtype of {}",
                        op.info.symbol,
                        lhs_type.pretty_print_color(),
                        sig.params[0].ty.pretty_print_color()
                    );
                    if closest_match.is_none() {
                        closest_match = Some(*op);
                    }
                    return false;
                }
                if !rhs_type.subtype(&sig.params[1].ty).success {
                    log::trace!(
                        "Skipping operator: {} because rhs type {} is not a subtype of {}",
                        op.info.symbol,
                        rhs_type.pretty_print_color(),
                        sig.params[1].ty.pretty_print_color()
                    );
                    closest_match = Some(*op);
                    return false;
                }
                true
            })
            .collect::<Vec<_>>();
        let selected_op = matching_ops.iter().copied().find(|candidate| {
            matching_ops.iter().all(|other| {
                more_specific_operator(candidate, other)
                    || !more_specific_operator(other, candidate)
            })
        });
        if let Some(op) = selected_op {
            log::trace!(
                "Found operator {} : ({} <: {}) -> ({} <: {}) -> {}",
                op.info.symbol.clone().yellow(),
                lhs_type.pretty_print_color(),
                op.signature().params[0].ty.pretty_print_color(),
                rhs_type.pretty_print_color(),
                op.signature().params[1].ty.pretty_print_color(),
                op.signature().ret.pretty_print_color()
            );
            match &op.handler {
                OpHandler::Runtime(RuntimeOpHandler { function_name, .. }) => {
                    let variants = self.lookup_function(function_name).ok_or_else(|| {
                        TypeError::new(
                            format!(
                                "Unknown handler function {} for operator {}",
                                function_name.clone().yellow(),
                                op_info.symbol.clone().yellow()
                            ),
                            info.clone(),
                        )
                    })?;
                    let function_ty = self.select_operator_handler_variant(
                        function_name,
                        variants,
                        &op.signature(),
                        info,
                    )?;

                    // Assert that the function type has two parameters and the return type
                    let FunctionType {
                        return_type: inner_ret,
                        ..
                    } = function_ty.borrow();
                    let Type::Function(inner_ty) = inner_ret else {
                        return Err(TypeError::new(
                            format!(
                                "Expected function type, found {}",
                                inner_ret.pretty_print_color()
                            ),
                            info.clone(),
                        )
                        .into());
                    };
                    let FunctionType {
                        return_type: outer_ret,
                        ..
                    } = inner_ty.borrow();

                    let result = CheckedAst::FunctionCall {
                        expr: Box::new(CheckedAst::FunctionCall {
                            expr: Box::new(CheckedAst::Identifier {
                                name: function_name.clone(),
                                ty: Type::Function(Box::new(function_ty.clone())),
                                info: info.clone(),
                            }),
                            arg: Box::new(checked_lhs),
                            ret_ty: inner_ret.clone(),
                            info: info.clone(),
                        }),
                        arg: Box::new(checked_rhs),
                        ret_ty: outer_ret.clone(),
                        info: info.clone(),
                    };

                    log::trace!(
                        "Binary: {} : {}",
                        result.print_expr(),
                        result.get_type().pretty_print_color()
                    );

                    return Ok(result);
                }
                OpHandler::Static(StaticOpHandler { handler, .. }) => {
                    log::trace!("Static operator: {}", op_info.symbol);
                    // Evaluate the handler at compile-time
                    return self
                        .check_expr(&handler(StaticOpAst::Infix(lhs.clone(), rhs.clone()))?);
                }
            }
        }
        let mut err = TypeError::new(
            format!(
                "Unknown binary operator {} {} {}",
                lhs_type.pretty_print_color(),
                op_info.symbol.clone().yellow(),
                rhs_type.pretty_print_color()
            ),
            info.clone(),
        )
        .with_label(
            format!(
                "This is of type {}",
                checked_lhs.get_type().pretty_print_color()
            ),
            lhs.info().clone(),
        )
        .with_label(
            format!(
                "This is of type {}",
                checked_rhs.get_type().pretty_print_color()
            ),
            rhs.info().clone(),
        );
        if let Some(closest_match) = closest_match {
            err = err.with_hint(format!(
                "Did you mean {} {} {} returning {}?",
                closest_match.signature().params[0].ty.pretty_print_color(),
                closest_match.info.symbol.clone().yellow(),
                closest_match.signature().params[1].ty.pretty_print_color(),
                closest_match.signature().ret.pretty_print_color()
            ));
        }
        Err(err.into())
    }

    fn check_unary(
        &mut self,
        op_info: &OpInfo,
        operand: &Ast,
        info: &LineInfo,
    ) -> TypeCheckerResult<CheckedAst> {
        let checked_operand = self.check_expr(operand)?;
        let operand_type = checked_operand.get_type();
        let mut closest_match = None;
        let matching_ops = self
            .lookup_operator(&op_info.symbol)
            .into_iter()
            .filter(|op| {
                closest_match = Some(*op);
                operand_type.subtype(&op.signature().params[0].ty).success
            })
            .collect::<Vec<_>>();
        let selected_op = matching_ops.iter().copied().find(|candidate| {
            matching_ops.iter().all(|other| {
                more_specific_operator(candidate, other)
                    || !more_specific_operator(other, candidate)
            })
        });
        if let Some(op) = selected_op {
            match &op.handler {
                OpHandler::Runtime(RuntimeOpHandler { function_name, .. }) => {
                    let variants = self.lookup_function(function_name).ok_or_else(|| {
                        TypeError::new(
                            format!(
                                "Unknown handler function {} for operator {}",
                                function_name.clone().yellow(),
                                op_info.symbol.clone().yellow()
                            ),
                            info.clone(),
                        )
                    })?;
                    let function_ty = self.select_operator_handler_variant(
                        function_name,
                        variants,
                        &op.signature(),
                        info,
                    )?;
                    let call = CheckedAst::FunctionCall {
                        ret_ty: function_ty.return_type.clone(),
                        expr: Box::new(CheckedAst::Identifier {
                            name: function_name.clone(),
                            ty: Type::Function(Box::new(function_ty)),
                            info: info.clone(),
                        }),
                        arg: Box::new(checked_operand),
                        info: info.clone(),
                    };
                    return Ok(call);
                }
                OpHandler::Static(StaticOpHandler { handler, .. }) => {
                    // Evaluate the handler at compile-time
                    return self.check_expr(&handler(StaticOpAst::Prefix(operand.clone()))?);
                }
            }
        }
        let mut err = TypeError::new(
            format!("Unknown unary operator {}", op_info.symbol.clone().yellow()),
            info.clone(),
        );
        if let Some(closest_match) = closest_match {
            err = err.with_hint(format!(
                "Did you mean {} {} returning {}?",
                closest_match.info.symbol.clone().yellow(),
                closest_match.signature().params[0].ty.pretty_print_color(),
                closest_match.signature().ret.pretty_print_color()
            ));
        }
        Err(err.into())
    }

    /// TODO: Check the binding pattern against the expression type.
    /// TODO: Create a new CheckedBindPattern that contains the type information for each variable in the pattern.
    fn _check_binding_pattern(
        &mut self,
        pattern: &BindPattern,
        expr_ty: &Type,
        info: &LineInfo,
    ) -> TypeCheckerResult<()> {
        match pattern {
            BindPattern::Variable { name, .. } => {
                self.add_variable(name.clone(), expr_ty.clone());
                Ok(())
            }
            BindPattern::MemberAccess { .. } => {
                if let Some(name) = pattern.binding_name() {
                    self.add_variable(name, expr_ty.clone());
                }
                Ok(())
            }
            BindPattern::Tuple { elements, .. } => {
                if let Type::Tuple(types) = expr_ty {
                    if elements.len() != types.len() {
                        return Err(TypeError::new(
                            format!(
                                "Tuple pattern has {} elements, but the type has {} elements",
                                elements.len(),
                                types.len()
                            ),
                            info.clone(),
                        )
                        .with_label(
                            format!("This pattern has {} elements", elements.len()),
                            pattern.info().clone(),
                        )
                        .with_label(
                            format!("This type has {} elements", types.len()),
                            info.clone(),
                        )
                        .into());
                    }
                    for (element, ty) in elements.iter().zip(types) {
                        self._check_binding_pattern(element, ty, info)?;
                    }
                    Ok(())
                } else {
                    Err(TypeError::new(
                        format!(
                            "Cannot match tuple pattern with non-tuple type {}",
                            expr_ty.pretty_print_color()
                        ),
                        info.clone(),
                    )
                    .with_label("This is not a tuple type".to_string(), info.clone())
                    .into())
                }
            }
            BindPattern::Record { fields, .. } => {
                if let Type::Record(types) = expr_ty {
                    for (key, pattern) in fields {
                        if let Some((_, ty)) = types.iter().find(|(k, _)| k == key) {
                            self._check_binding_pattern(pattern, ty, info)?;
                        } else {
                            return Err(TypeError::new(
                                format!(
                                    "Field {} not found in record type {}",
                                    key.to_string().yellow(),
                                    expr_ty.pretty_print_color()
                                ),
                                info.clone(),
                            )
                            .with_label(
                                format!(
                                    "This record does not have the field {}",
                                    key.to_string().yellow()
                                ),
                                pattern.info().clone(),
                            )
                            .into());
                        }
                    }
                    Ok(())
                } else {
                    Err(TypeError::new(
                        format!(
                            "Cannot match record pattern with non-record type {}",
                            expr_ty.pretty_print_color()
                        ),
                        info.clone(),
                    )
                    .with_label("This is not a record type".to_string(), info.clone())
                    .into())
                }
            }
            BindPattern::List { elements, .. } => {
                if let Type::List(element_type) = expr_ty {
                    for element in elements {
                        self._check_binding_pattern(element, element_type, info)?;
                    }
                    Ok(())
                } else {
                    Err(TypeError::new(
                        format!(
                            "Cannot match list pattern with non-list type {}",
                            expr_ty.pretty_print_color()
                        ),
                        info.clone(),
                    )
                    .with_label("This is not a list type".to_string(), info.clone())
                    .into())
                }
            }
            BindPattern::Wildcard => Ok(()),
            BindPattern::Literal { value, .. } => {
                let value = value.as_value();
                let value_type = value.get_type();
                if !value_type.subtype(expr_ty).success {
                    return Err(TypeError::new(
                        format!(
                            "Literal pattern of type {} does not match type {}",
                            value_type.pretty_print_color(),
                            expr_ty.pretty_print_color()
                        ),
                        info.clone(),
                    )
                    .with_label(
                        format!("This is of type {}", value_type.pretty_print_color()),
                        pattern.info().clone(),
                    )
                    .into());
                }
                Ok(())
            }
            BindPattern::Rest { .. } => Ok(()),
        }
    }

    // ================== Type inference functions ==================
}

/// Extract all names and their types from a binding pattern
fn binding_typed_names(pattern: &BindPattern, ty: &Type) -> HashSet<(String, Type)> {
    fn visit(pattern: &BindPattern, ty: &Type, names: &mut HashSet<(String, Type)>) {
        match pattern {
            BindPattern::Variable { name, .. } => {
                names.insert((name.clone(), ty.clone()));
            }
            BindPattern::MemberAccess { .. } => {
                if let Some(name) = pattern.binding_name() {
                    names.insert((name, ty.clone()));
                }
            }
            BindPattern::Tuple { elements, .. } => {
                if let Type::Tuple(types) = ty {
                    for (element, t) in elements.iter().zip(types) {
                        visit(element, t, names);
                    }
                }
            }
            BindPattern::Record { fields, .. } => {
                if let Type::Record(types) = ty {
                    for (key, pattern) in fields {
                        if let Some((_, t)) = types.iter().find(|(k, _)| k == key) {
                            visit(pattern, t, names);
                        }
                    }
                }
            }
            BindPattern::List { elements, .. } => {
                if let Type::List(element_type) = ty {
                    for element in elements {
                        visit(element, &element_type, names);
                    }
                }
            }
            BindPattern::Wildcard | BindPattern::Rest { .. } | BindPattern::Literal { .. } => {}
        }
    }
    let mut names = HashSet::new();
    visit(pattern, ty, &mut names);
    names
}

fn more_specific_operator(candidate: &Operator, other: &Operator) -> bool {
    let candidate = candidate.signature();
    let other = other.signature();
    candidate.params.len() == other.params.len()
        && candidate
            .params
            .iter()
            .zip(other.params.iter())
            .all(|(candidate, other)| candidate.ty.subtype(&other.ty).success)
}

fn number_to_usize(n: &Number) -> Option<usize> {
    match n {
        Number::UnsignedInteger(u) => match u {
            UnsignedInteger::UInt1(v) => Some((*v).into()),
            UnsignedInteger::UInt8(v) => Some((*v).into()),
            UnsignedInteger::UInt16(v) => Some((*v).into()),
            UnsignedInteger::UInt32(v) => usize::try_from(*v).ok(),
            UnsignedInteger::UInt64(v) => usize::try_from(*v).ok(),
            UnsignedInteger::UInt128(v) => usize::try_from(*v).ok(),
            UnsignedInteger::UIntVar(v) => v.to_string().parse::<usize>().ok(),
        },
        _ => None,
    }
}
