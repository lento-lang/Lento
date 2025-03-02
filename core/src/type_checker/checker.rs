use std::{borrow::Borrow, collections::HashMap};

use crate::{
    interpreter::value::{RecordKey, Value},
    lexer::token::LineInfo,
    parser::{
        ast::{Ast, ParamAst, TypeAst},
        op::{
            Operator, OperatorHandler, OperatorInfo, RuntimeOperatorHandler, StaticOperatorAst,
            StaticOperatorHandler,
        },
    },
};

use super::{
    checked_ast::{CheckedAst, CheckedParam},
    types::{std_types, FunctionType, GetType, Type, TypeTrait},
};

/// A type error is an error that occurs during type checking.
#[derive(Debug)]
pub struct TypeError {
    pub message: String,
    pub info: LineInfo,
}

impl TypeError {
    pub fn new(message: String, info: LineInfo) -> Self {
        Self { message, info }
    }
}

// The result of the type checker stage
pub type TypeResult<T> = Result<T, TypeError>;

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

    fn lookup_static_operator(&self, symbol: &str) -> Option<&StaticOperatorHandler> {
        let operator: Option<&StaticOperatorHandler> = self.env.operators.iter().find_map(|o| {
            if o.info.symbol == symbol && o.info.is_static {
                let OperatorHandler::Static(op) = &o.handler else {
                    unreachable!("Operator is not static");
                };
                Some(op)
            } else {
                None
            }
        });
        let operator =
            operator.or_else(|| self.parent.and_then(|p| p.lookup_static_operator(symbol)));
        operator
    }

    // ================== Scanning functions ==================

    fn scan_forward(&mut self, expr: &[Ast]) -> TypeResult<()> {
        for e in expr {
            if let Ast::Assignment {
                target,
                expr,
                info: _,
            } = e
            {
                let Ast::Identifier { name, .. } = target.borrow() else {
                    continue;
                };
                match expr.borrow() {
                    Ast::FunctionDef {
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
                            "Adding function '{}' with variation: {}",
                            name,
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
            Ast::FunctionDef {
                param,
                body,
                return_type,
                info,
            } => self.check_function(self.check_param(param)?, body, return_type, info)?,
            Ast::Literal { value, info } => CheckedAst::Literal {
                value: value.clone(),
                info: info.clone(),
            },
            Ast::Tuple { exprs, info } => self.check_tuple(exprs, info)?,
            Ast::List { exprs: elems, info } => self.check_list(elems, info)?,
            Ast::Record { fields, info } => self.check_record(fields, info)?,
            Ast::Identifier { name, info } => self.check_identifier(name, info)?,
            Ast::FunctionCall {
                expr,
                arg: args,
                info,
            } => self.check_call(expr, args, info)?,
            Ast::Accumulate {
                op_info: op,
                exprs: operands,
                info,
            } => self.check_accumulate(op, operands, info)?,
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
            Ast::Assignment { target, expr, info } => self.check_assignment(target, expr, info)?,
            Ast::Block { exprs, info } => self.check_block(exprs, info)?,
        })
    }

    fn check_type_expr(&self, expr: &TypeAst) -> TypeResult<Type> {
        Ok(match expr {
            TypeAst::Identifier(name, info) => self
                .lookup_type(name)
                .cloned()
                .ok_or_else(|| TypeError::new(format!("Unknown type '{}'", name), info.clone()))?,
        })
    }

    fn check_param(&self, param: &ParamAst) -> TypeResult<CheckedParam> {
        let param_ty = if let Some(ty) = &param.ty {
            self.check_type_expr(ty)?
        } else {
            return Err(TypeError::new(
                format!("Parameter type for '{}' is missing", param.name),
                param.info.clone(),
            ));
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
        let body_info = body.info();
        let body = self.new_scope().check_expr(body)?;
        let body_type = body.get_type().clone();
        let return_type = if let Some(ty) = &return_type {
            let ty = self.check_type_expr(ty)?;
            if !ty.subtype(&body_type).success {
                return Err(TypeError::new(format!(
                        "Function body type does not match the return type. Expected '{}', found '{}'",
                        ty, &body_type
                    ),
					body_info.clone(),
				));
            }
            ty
        } else {
            // Infer the return type from the body
            body_type
        };

        Ok(CheckedAst::function_def(
            param,
            body,
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
                        format!("Function '{}' has no variants", name),
                        info.clone(),
                    ));
                }
            }
            None => {
                return Err(TypeError::new(
                    format!("Unknown variable '{}'", name),
                    info.clone(),
                ));
            }
        })
    }

    fn check_assignment(
        &mut self,
        target: &Ast,
        expr: &Ast,
        info: &LineInfo,
    ) -> TypeResult<CheckedAst> {
        let target = match target {
            Ast::Identifier { name, .. } => name,
            _ => {
                return Err(TypeError::new(
                    "Assignment expects an identifier".to_string(),
                    info.clone(),
                ))
            }
        };
        if let Some(existing) = self.lookup_local_identifier(target) {
            let ty_name = match existing {
                IdentifierType::Variable(_) => "Variable",
                IdentifierType::Type(_) => "Type",
                IdentifierType::Function(_) => "Function",
            };
            return Err(TypeError::new(
                format!("{} '{}' already exists", ty_name, target),
                info.clone(),
            ));
        }
        let expr = self.check_expr(expr)?;
        let ty = expr.get_type().clone();
        let assign_info = info.join(expr.info());
        self.env.add_variable(target, ty.clone());
        Ok(CheckedAst::Assignment {
            target: Box::new(CheckedAst::Identifier {
                name: target.to_string(),
                ty: ty.clone(),
                info: info.clone(),
            }),
            expr: Box::new(expr),
            ty,
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
                ))
            }
        } else {
            Err(TypeError::new(
                format!("Cannot call non-function: {}", expr.get_type()),
                info.clone(),
            ))
        }
    }

    /// Check the type of an accumulate expression.
    /// An accumulate expression results in a function variation-specific call.
    fn check_accumulate(
        &mut self,
        op_info: &OperatorInfo,
        operands: &[Ast],
        info: &LineInfo,
    ) -> TypeResult<CheckedAst> {
        let checked_operands = operands
            .iter()
            .map(|a| self.check_expr(a))
            .collect::<TypeResult<Vec<_>>>()?;
        if let Some(op) = self
            .lookup_operator(&op_info.symbol)
            .into_iter()
            .find(|op| {
                checked_operands
                    .iter()
                    .zip(op.signature().params.iter())
                    .all(|(operand, param)| operand.get_type().subtype(&param.ty).success)
            })
        {
            match &op.handler {
                OperatorHandler::Runtime(RuntimeOperatorHandler { function_name, .. }) => {
                    // Construct a function call expression
                    let mut operands = operands.iter();
                    let mut call = Ast::FunctionCall {
                        expr: Box::new(Ast::Identifier {
                            name: function_name.clone(),
                            info: info.clone(),
                        }),
                        arg: Box::new(operands.next().unwrap().clone()),
                        info: info.clone(),
                    };
                    for arg in operands {
                        call = Ast::FunctionCall {
                            expr: Box::new(call),
                            arg: Box::new(arg.clone()),
                            info: info.clone(),
                        };
                    }
                    self.check_expr(&call)
                }
                OperatorHandler::Static(StaticOperatorHandler { handler, .. }) => {
                    // Evaluate the handler at compile-time
                    let ast = handler(StaticOperatorAst::Accumulate(operands.to_vec()));
                    self.check_expr(&ast)
                }
            }
        } else {
            Err(TypeError::new(
                format!("Unknown accumulate operator '{}'", op_info.symbol),
                info.clone(),
            ))
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
        op_info: &OperatorInfo,
        rhs: &Ast,
        info: &LineInfo,
    ) -> TypeResult<CheckedAst> {
        if let Some(op) = self.lookup_static_operator(&op_info.symbol) {
            log::trace!("Found static operator: {}", op_info.symbol);
            return self.check_expr(&(op.handler)(StaticOperatorAst::Infix(
                lhs.clone(),
                rhs.clone(),
            )));
        }
        let checked_lhs = self.check_expr(lhs)?;
        let checked_rhs = self.check_expr(rhs)?;
        let lhs_type = checked_lhs.get_type();
        let rhs_type = checked_rhs.get_type();
        for op in self.lookup_operator(&op_info.symbol) {
            if !lhs_type.subtype(&op.signature().params[0].ty).success {
                log::trace!(
                    "Skipping operator: {} because lhs type {} is not a subtype of {}",
                    op.info.symbol,
                    lhs_type.pretty_print_color(),
                    op.signature().params[0].ty.pretty_print_color()
                );
                continue;
            }
            if !rhs_type.subtype(&op.signature().params[1].ty).success {
                log::trace!(
                    "Skipping operator: {} because rhs type {} is not a subtype of {}",
                    op.info.symbol,
                    rhs_type.pretty_print_color(),
                    op.signature().params[1].ty.pretty_print_color()
                );
                continue;
            }
            log::trace!(
                "Found operator: '{}' : ({} <: {}) -> ({} <: {}) -> {}",
                op.info.symbol,
                lhs_type.pretty_print_color(),
                op.signature().params[0].ty.pretty_print_color(),
                rhs_type.pretty_print_color(),
                op.signature().params[1].ty.pretty_print_color(),
                op.signature().ret.pretty_print_color()
            );
            match &op.handler {
                OperatorHandler::Runtime(RuntimeOperatorHandler { function_name, .. }) => {
                    let function_ty = self
                        .lookup_function(function_name)
                        .ok_or_else(|| {
                            TypeError::new(
                                format!("Unknown function '{}'", function_name),
                                info.clone(),
                            )
                        })?
                        .first()
                        .ok_or_else(|| {
                            TypeError::new(
                                format!("Function '{}' has no variants", function_name),
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
                            format!("Expected function type, found '{}'", inner_ret),
                            info.clone(),
                        ));
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
                OperatorHandler::Static(StaticOperatorHandler { handler, .. }) => {
                    log::trace!("Static operator: {}", op_info.symbol);
                    // Evaluate the handler at compile-time
                    let ast = handler(StaticOperatorAst::Infix(lhs.clone(), rhs.clone()));
                    return self.check_expr(&ast);
                }
            }
        }
        Err(TypeError::new(
            format!("Unknown binary operator '{}'", op_info.symbol),
            info.clone(),
        ))
    }

    fn check_unary(
        &mut self,
        op_info: &OperatorInfo,
        operand: &Ast,
        info: &LineInfo,
    ) -> TypeResult<CheckedAst> {
        let checked_operand = self.check_expr(operand)?;
        let operand_type = checked_operand.get_type();
        for op in self.lookup_operator(&op_info.symbol) {
            if !op.signature().params[0].ty.subtype(operand_type).success {
                continue;
            }
            match &op.handler {
                OperatorHandler::Runtime(RuntimeOperatorHandler { function_name, .. }) => {
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
                OperatorHandler::Static(StaticOperatorHandler { handler, .. }) => {
                    // Evaluate the handler at compile-time
                    let ast = handler(StaticOperatorAst::Prefix(operand.clone()));
                    return self.check_expr(&ast);
                }
            }
        }
        Err(TypeError::new(
            format!("Unknown unary operator '{}'", op_info.symbol),
            info.clone(),
        ))
    }

    // ================== Type inference functions ==================
}
