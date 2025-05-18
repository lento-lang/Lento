use std::{borrow::Borrow, collections::HashMap};

use colorful::Colorful;

use crate::{
    interpreter::value::{RecordKey, Value},
    parser::{
        ast::{Ast, ParamAst, TypeAst},
        error::ParseError,
        op::{OpHandler, OpInfo, Operator, RuntimeOpHandler, StaticOpAst, StaticOpHandler},
        pattern::BindPattern,
    },
    util::error::{BaseError, BaseErrorExt, LineInfo},
};

use super::{
    checked_ast::{CheckedAst, CheckedParam},
    types::{std_types, FunctionType, GetType, Type, TypeTrait},
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
    /// this should only happen iff a static operator handler is used.
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
pub type TypeResult<T> = Result<T, TypeErrorVariant>;

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

    // Add a variable to the type environment
    pub fn add_variable(&mut self, name: &str, ty: Type) {
        self.variables.insert(name.to_string(), ty);
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

    pub fn add_operator(&mut self, op: Operator) {
        self.env.add_operator(op);
    }

    pub fn add_function(&mut self, name: &str, variation: FunctionType) {
        self.env.add_function(name.to_string(), variation);
    }

    fn new_scope(&self) -> TypeChecker {
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

    fn lookup_identifier(&self, name: &str) -> Option<IdentifierType> {
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

    fn lookup_local_identifier(&self, name: &str) -> Option<IdentifierType> {
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

    // ================== Scanning functions ==================

    fn scan_forward(&mut self, expr: &[Ast]) -> TypeResult<()> {
        for e in expr {
            if let Ast::Assignment { target, expr, .. } = e {
                let BindPattern::Variable { name, .. } = target else {
                    continue;
                };
                match expr.borrow() {
                    Ast::Lambda {
                        param,
                        body,
                        return_type,
                        info,
                    } => {
                        let checked_param = self.check_param(param)?;
                        let checked =
                            self.check_function(checked_param.clone(), body, return_type, info)?;
                        let variation = FunctionType {
                            param: checked_param,
                            return_type: checked.get_type().clone(),
                        };
                        log::debug!(
                            "Adding function {} with variation {}",
                            name.clone().yellow(),
                            variation.pretty_print()
                        );
                        self.env.add_function(name.clone(), variation);
                    }
                    _ => {
                        let checked = self.check_expr(expr)?;
                        self.env.add_variable(name, checked.get_type().clone());
                    }
                }
            }
        }
        Ok(())
    }

    // ================== Type checking functions ==================

    pub fn check_top_exprs(&mut self, exprs: &[Ast]) -> TypeResult<Vec<CheckedAst>> {
        self.scan_forward(exprs)?;
        let mut res = vec![];
        for e in exprs {
            res.push(self.check_expr(e)?);
        }
        Ok(res)
    }

    /// Check the type of an expression
    pub fn check_expr(&mut self, expr: &Ast) -> TypeResult<CheckedAst> {
        Ok(match expr {
            Ast::Lambda {
                param,
                body,
                return_type,
                info,
            } => self.check_function(self.check_param(param)?, body, return_type, info)?,
            Ast::Literal { value, info } => CheckedAst::Literal {
                value: value.clone(),
                info: info.clone(),
            },
            Ast::LiteralType { expr } => self.check_literal_type(expr)?,
            Ast::Tuple { exprs, info } => self.check_tuple(exprs, info)?,
            Ast::List { exprs: elems, info } => self.check_list(elems, info)?,
            Ast::Record { fields, info } => self.check_record(fields, info)?,
            Ast::MemderAccess {
                expr: record,
                field,
                info,
            } => self.check_field_access(record, field, info)?,
            Ast::Identifier { name, info } => self.check_identifier(name, info)?,
            Ast::FunctionCall {
                expr,
                arg: args,
                info,
            } => self.check_call(expr, args, info)?,
            Ast::Binary {
                lhs,
                op_info,
                rhs,
                info,
            } => self.check_binary(lhs, op_info, rhs, info)?,
            Ast::Unary {
                op_info: op,
                expr: operand,
                info,
            } => self.check_unary(op, operand, info)?,
            Ast::Assignment {
                annotation,
                target,
                expr,
                info,
            } => self.check_assignment(annotation, target, expr, info)?,
            Ast::Block { exprs, info } => self.check_block(exprs, info)?,
        })
    }

    fn check_type_expr(&self, expr: &TypeAst) -> TypeResult<Type> {
        Ok(match expr {
            TypeAst::Identifier { name, info } => {
                self.lookup_type(name).cloned().ok_or_else(|| {
                    TypeError::new(format!("Unknown type {}", name.clone().red()), info.clone())
                        .with_label("This type is not defined".to_string(), info.clone())
                })?
            }
            TypeAst::Constructor { expr, args, info } => {
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
                let args = args
                    .iter()
                    .map(|a| self.check_type_expr(a))
                    .collect::<TypeResult<Vec<_>>>()?;
                Type::Constructor(base_name, args, base_type)
            }
        })
    }

    fn check_literal_type(&self, expr: &TypeAst) -> TypeResult<CheckedAst> {
        let info = expr.info();
        let ty = self.check_type_expr(expr)?;
        Ok(CheckedAst::LiteralType {
            value: ty,
            info: info.clone(),
        })
    }

    fn check_param(&self, param: &ParamAst) -> TypeResult<CheckedParam> {
        let param_ty = if let Some(ty) = &param.ty {
            self.check_type_expr(ty)?
        } else {
            return Err(TypeError::new(
                format!("Missing parameter type for {}", param.name.clone().yellow()),
                param.info.clone(),
            )
            .with_label(
                "Add a type to this parameter".to_string(),
                param.info.clone(),
            )
            .into());
        };
        let param = CheckedParam {
            name: param.name.clone(),
            ty: param_ty,
        };
        Ok(param)
    }

    fn check_function(
        &mut self,
        param: CheckedParam,
        body: &Ast,
        return_type: &Option<TypeAst>,
        info: &LineInfo,
    ) -> TypeResult<CheckedAst> {
        let checked_body = self.new_scope().check_expr(body)?;
        let body_type = checked_body.get_type().clone();
        let return_type = if let Some(ty) = &return_type {
            let ty = self.check_type_expr(ty)?;
            if !ty.subtype(&body_type).success {
                return Err(TypeError::new(
                    format!(
                        "Function body type does not match the return type. Expected {}, found {}",
                        ty.pretty_print_color(),
                        body_type.pretty_print_color()
                    ),
                    info.clone(),
                )
                .with_label(
                    format!("This is not of type {}", ty.pretty_print_color()),
                    body.last_info().clone(),
                )
                .into());
            }
            ty
        } else {
            // Infer the return type from the body
            body_type
        };

        Ok(CheckedAst::function_def(
            param,
            checked_body,
            return_type,
            info.clone(),
        ))
    }

    fn check_tuple(&mut self, elems: &[Ast], info: &LineInfo) -> TypeResult<CheckedAst> {
        if elems.is_empty() {
            return Ok(CheckedAst::Tuple {
                exprs: vec![],
                expr_types: std_types::UNIT,
                info: info.clone(),
            });
        }
        let checked_elems = self.check_top_exprs(elems)?;
        let elem_types = checked_elems
            .iter()
            .map(|e| e.get_type())
            .cloned()
            .collect::<Vec<_>>();
        Ok(CheckedAst::Tuple {
            exprs: checked_elems,
            expr_types: Type::Tuple(elem_types),
            info: info.clone(),
        })
    }

    fn check_list(&mut self, elems: &[Ast], info: &LineInfo) -> TypeResult<CheckedAst> {
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

    fn check_record(
        &mut self,
        pairs: &[(RecordKey, Ast)],
        info: &LineInfo,
    ) -> TypeResult<CheckedAst> {
        let pairs = pairs
            .iter()
            .map(|(k, v)| Ok((k.clone(), self.check_expr(v)?)))
            .collect::<TypeResult<Vec<_>>>()?;
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
    ) -> TypeResult<CheckedAst> {
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

    fn check_identifier(&self, name: &str, info: &LineInfo) -> TypeResult<CheckedAst> {
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
            Some(IdentifierType::Function(variants)) => {
                // TODO: Do not select the first variant!!!
                // Instead, select the variant that matches the arguments types
                // Or infer based on the context of use etc.
                // This is a very temporary solution...
                if let Some(variant) = variants.first() {
                    CheckedAst::Identifier {
                        name: name.to_string(),
                        ty: Type::Function(Box::new(variant.clone())),
                        info: info.clone(),
                    }
                } else {
                    return Err(TypeError::new(
                        format!("Function {} has no variants", name.yellow()),
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

    fn check_assignment(
        &mut self,
        annotation: &Option<TypeAst>,
        target: &BindPattern,
        expr: &Ast,
        info: &LineInfo,
    ) -> TypeResult<CheckedAst> {
        let target_name = match target {
            BindPattern::Variable { name, .. } => name,
            BindPattern::Tuple { .. }
            | BindPattern::Record { .. }
            | BindPattern::List { .. }
            | BindPattern::Wildcard
            | BindPattern::Literal { .. }
            | BindPattern::Rest { .. } => {
                let checked_expr = self.check_expr(expr)?;
                let expr_type = checked_expr.get_type().clone();
                self.check_binding_pattern(target, &expr_type, info)?;
                return Ok(CheckedAst::Assignment {
                    target: target.clone(),
                    expr: Box::new(checked_expr),
                    info: info.clone(),
                });
            }
            BindPattern::Function {
                name: _,
                annotation: _,
                params: _,
                info: _,
            } => todo!("Function assignment"),
        };
        if let Some(existing) = self.lookup_local_identifier(target_name) {
            let ty_name = match existing {
                IdentifierType::Variable(_) => "Variable",
                IdentifierType::Type(_) => "Type",
                IdentifierType::Function(_) => "Function",
            };
            return Err(TypeError::new(
                format!(
                    "{} {} already exists",
                    ty_name,
                    target_name.clone().yellow()
                ),
                info.clone(),
            )
            .with_hint("Use a different name for the variable".to_string())
            .into());
        }
        let expr = self.check_expr(expr)?;
        let body_ty = expr.get_type().clone();
        if let Some(ann) = annotation {
            let ann_ty = self.check_type_expr(ann)?;
            if !body_ty.subtype(&ann_ty).success {
                return Err(TypeError::new(
                    format!(
                        "{} is not a valid subtype of {}",
                        expr.pretty_print(),
                        ann_ty.pretty_print_color(),
                    ),
                    info.clone(),
                )
                .with_label(
                    format!(
                        "This should be of type {} but is {}",
                        ann_ty.pretty_print_color(),
                        body_ty.pretty_print_color()
                    ),
                    expr.info().clone(),
                )
                .into());
            }
        }
        let assign_info = info.join(expr.info());
        self.env.add_variable(target_name, body_ty.clone());
        Ok(CheckedAst::Assignment {
            target: BindPattern::Variable {
                annotation: None, // TODO: Add annotation support!!!
                name: target_name.to_string(),
                info: target.info().clone(),
            },
            expr: Box::new(expr),
            info: assign_info,
        })
    }

    fn check_block(&mut self, exprs: &[Ast], info: &LineInfo) -> TypeResult<CheckedAst> {
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

    fn check_call(&mut self, expr: &Ast, arg: &Ast, info: &LineInfo) -> TypeResult<CheckedAst> {
        // TODO: Add support for multiple function variants
        // TODO: This job should be done in the type checker
        // TODO: so that the interpreter can just call the function

        // Go through the expression and check if the type is a function
        let expr = self.check_expr(expr)?;
        if let Type::Function(fn_ty) = expr.get_type() {
            let FunctionType {
                param,
                return_type: ret,
            } = fn_ty.borrow();
            // Check the type of the argument
            let arg = self.check_expr(arg)?;
            let tr = arg.get_type().subtype(&param.ty);
            if tr.success {
                log::info!(
                    "Function call: {} : {} -> {} with argument {} : {}",
                    expr.print_sexpr(),
                    param.ty.pretty_print_color(),
                    ret.pretty_print_color(),
                    arg.pretty_print(),
                    arg.get_type().pretty_print_color()
                );
                if !tr.judgements.is_empty() {
                    log::trace!("Judgements: {}", tr.pretty_print_color_judgements());
                    let mut changed = false;
                    let param_ty = param.ty.specialize(&tr.judgements, &mut changed);
                    let ret_ty = ret.specialize(&tr.judgements, &mut changed);
                    if changed {
                        log::trace!(
                            "Specialized call: {} : {} -> {}",
                            expr.print_sexpr(),
                            param_ty.pretty_print_color(),
                            ret_ty.pretty_print_color()
                        );
                    }
                }

                Ok(CheckedAst::FunctionCall {
                    return_type: ret.clone(),
                    expr: Box::new(expr),
                    arg: Box::new(arg),
                    info: info.clone(),
                })
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
    ) -> TypeResult<CheckedAst> {
        if let Some(op) = self.lookup_static_operator(&op_info.symbol) {
            log::trace!("Found static operator: {}", op_info.symbol);
            return self.check_expr(&(op.handler)(StaticOpAst::Infix(lhs.clone(), rhs.clone()))?);
        }
        let checked_lhs = self.check_expr(lhs)?;
        let checked_rhs = self.check_expr(rhs)?;
        let lhs_type = checked_lhs.get_type();
        let rhs_type = checked_rhs.get_type();
        let mut closest_match = None;
        for op in self.lookup_operator(&op_info.symbol) {
            if !lhs_type.subtype(&op.signature().params[0].ty).success {
                log::trace!(
                    "Skipping operator: {} because lhs type {} is not a subtype of {}",
                    op.info.symbol,
                    lhs_type.pretty_print_color(),
                    op.signature().params[0].ty.pretty_print_color()
                );
                if closest_match.is_none() {
                    closest_match = Some(op);
                }
                continue;
            }
            if !rhs_type.subtype(&op.signature().params[1].ty).success {
                log::trace!(
                    "Skipping operator: {} because rhs type {} is not a subtype of {}",
                    op.info.symbol,
                    rhs_type.pretty_print_color(),
                    op.signature().params[1].ty.pretty_print_color()
                );
                closest_match = Some(op);
                continue;
            }
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
                    let function_ty = self
                        .lookup_function(function_name)
                        .ok_or_else(|| {
                            TypeError::new(
                                format!(
                                    "Unknown handler function {} for operator {}",
                                    function_name.clone().yellow(),
                                    op_info.symbol.clone().yellow()
                                ),
                                info.clone(),
                            )
                        })?
                        .first()
                        .ok_or_else(|| {
                            TypeError::new(
                                format!(
                                    "Handler function {} for operator {} has no variants",
                                    function_name.clone().yellow(),
                                    op_info.symbol.clone().yellow()
                                ),
                                info.clone(),
                            )
                        })?
                        .clone();

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
                            return_type: inner_ret.clone(),
                            info: info.clone(),
                        }),
                        arg: Box::new(checked_rhs),
                        return_type: outer_ret.clone(),
                        info: info.clone(),
                    };

                    log::trace!(
                        "Binary: {} : {}",
                        result.print_sexpr(),
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
    ) -> TypeResult<CheckedAst> {
        let checked_operand = self.check_expr(operand)?;
        let operand_type = checked_operand.get_type();
        let mut closest_match = None;
        for op in self.lookup_operator(&op_info.symbol) {
            closest_match = Some(op);
            if !op.signature().params[0].ty.subtype(operand_type).success {
                continue;
            }
            match &op.handler {
                OpHandler::Runtime(RuntimeOpHandler { function_name, .. }) => {
                    let call = Ast::FunctionCall {
                        expr: Box::new(Ast::Identifier {
                            name: function_name.clone(),
                            info: info.clone(),
                        }),
                        arg: Box::new(operand.clone()),
                        info: info.clone(),
                    };
                    return self.check_expr(&call);
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

    fn check_binding_pattern(
        &mut self,
        pattern: &BindPattern,
        expr_ty: &Type,
        info: &LineInfo,
    ) -> TypeResult<()> {
        match pattern {
            BindPattern::Variable { name, .. } => {
                self.env.add_variable(name, expr_ty.clone());
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
                        self.check_binding_pattern(element, ty, info)?;
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
                            self.check_binding_pattern(pattern, ty, info)?;
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
                        self.check_binding_pattern(element, element_type, info)?;
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
            BindPattern::Function { .. } => Err(TypeError::new(
                "Function binding patterns are not supported in this context".to_string(),
                info.clone(),
            )
            .with_label(
                "This is a function binding pattern".to_string(),
                pattern.info().clone(),
            )
            .into()),
        }
    }

    // ================== Type inference functions ==================
}
