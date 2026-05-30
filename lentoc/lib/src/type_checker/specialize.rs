use crate::{
    interpreter::number::{Number, UnsignedInteger},
    interpreter::value::RecordKey,
    parser::{
        ast::Ast,
        error::ParseError,
        parser::{EFFECT_ASCRIPTION_SYM, FN_ARROW_SYM, SUM_TYPE_SYM},
    },
    type_checker::checked_ast::{ArrayLenAst, Effect, TypeAst},
    util::error::{BaseErrorExt, LineInfo},
};
use colorful::Colorful;

pub fn into_type_ast(expr: Ast) -> Result<TypeAst, ParseError> {
    log::trace!(
        "Converting expression into TypeAst: {}",
        expr.print_expr().light_blue()
    );
    match expr {
        Ast::Identifier { name, info } => Ok(TypeAst::Identifier {
            name: name.clone(),
            info: info.clone(),
        }),
        Ast::List { mut exprs, info } if exprs.len() == 1 => {
            let elem = into_type_ast(exprs.remove(0))?;
            Ok(TypeAst::List {
                elem: Box::new(elem),
                info: info.clone(),
            })
        }
        Ast::Array { elem, len, info } => {
            let elem = into_type_ast(*elem)?;
            let len = array_len_from_ast(*len)?;
            Ok(TypeAst::Array {
                elem: Box::new(elem),
                len,
                info,
            })
        }
        Ast::Tuple { exprs, info } => {
            let items = exprs
                .into_iter()
                .map(into_type_ast)
                .collect::<Result<Vec<_>, ParseError>>()?;
            Ok(TypeAst::Tuple { items, info })
        }
        Ast::Record { fields, info } => {
            if let Some(refinement) = try_into_refinement(&fields, &info)? {
                return Ok(refinement);
            }
            let fields = fields
                .into_iter()
                .map(|(key, value)| Ok((key, into_type_ast(value)?)))
                .collect::<Result<Vec<_>, ParseError>>()?;
            Ok(TypeAst::Record { fields, info })
        }
        Ast::FunctionCall { expr, arg, info } => {
            let mut params = Vec::new();
            match *arg {
                Ast::Tuple { exprs, .. } => {
                    for arg_expr in exprs {
                        params.push(into_type_ast(arg_expr)?);
                    }
                }
                arg => params.push(into_type_ast(arg)?),
            }
            let mut head = *expr;
            while let Ast::FunctionCall { expr, arg, .. } = head {
                match *arg {
                    Ast::Tuple { exprs, .. } => {
                        for arg_expr in exprs.into_iter().rev() {
                            params.push(into_type_ast(arg_expr)?);
                        }
                    }
                    arg => params.push(into_type_ast(arg)?),
                }
                head = *expr;
            }
            params.reverse();
            let head = into_type_ast(head)?;
            Ok(TypeAst::Application {
                expr: Box::new(head),
                args: params,
                info,
            })
        }
        Ast::Binary { lhs, op, rhs, info } if op.symbol == SUM_TYPE_SYM => {
            let lhs = into_type_ast(*lhs)?;
            let rhs = into_type_ast(*rhs)?;
            let mut variants = Vec::new();
            match lhs {
                TypeAst::Sum { variants: lhs, .. } => variants.extend(lhs),
                lhs => variants.push(lhs),
            }
            match rhs {
                TypeAst::Sum { variants: rhs, .. } => variants.extend(rhs),
                rhs => variants.push(rhs),
            }
            Ok(TypeAst::Sum { variants, info })
        }
        Ast::Binary { lhs, op, rhs, info } if op.symbol == FN_ARROW_SYM => {
            // Effect ascription attaches to the return side:
            //   `A -> B ! IO`  parses as  `A -> (B ! IO)`
            // The `!` binds tighter than `->` (prec 750 vs 650), so the
            // effect is always on the rhs of the arrow; a `!` on the lhs
            // would be `(A ! IO) -> B`, which is semantically invalid
            // (input types do not have effects).
            let mut eff = vec![];

            let lhs = into_type_ast(*lhs)?;

            let rhs = match *rhs {
                Ast::Binary {
                    lhs: eff_lhs,
                    op: eff_op,
                    rhs: eff_rhs,
                    ..
                } if eff_op.symbol == EFFECT_ASCRIPTION_SYM => {
                    eff = into_effect_ast(*eff_rhs)?;
                    into_type_ast(*eff_lhs)?
                }
                rhs => into_type_ast(rhs)?,
            };

            Ok(TypeAst::Lambda {
                lhs: Box::new(lhs),
                rhs: Box::new(rhs),
                eff,
                info,
            })
        }
        Ast::Binary { op, info, .. } if op.symbol == EFFECT_ASCRIPTION_SYM => Err(ParseError::new(
            "Effect ascription is only valid on function types".to_string(),
            info,
        )),
        Ast::Block { exprs, info } => {
            let mut items = Vec::new();
            for expr in exprs {
                items.push(into_type_ast(expr)?);
            }
            if items.len() == 1 {
                Ok(items.remove(0))
            } else {
                Ok(TypeAst::Tuple { items, info })
            }
        }
        Ast::Literal { value, info } => {
            if matches!(value, crate::interpreter::value::Value::Type(_)) {
                return Err(ParseError::new(
                    "Expected a value literal singleton type, found a type literal".to_string(),
                    info,
                ));
            }
            Ok(TypeAst::Literal { value, info })
        }
        _ => Err(ParseError::new(
            format!("Expected a type expression, found: {}", expr.print_expr()),
            expr.info().clone(),
        )),
    }
}

fn into_effect_ast(effect_expr: Ast) -> Result<Vec<Effect>, ParseError> {
    match effect_expr {
        Ast::Identifier { name, info: _ } => Ok(vec![Effect {
            name,
            params: vec![],
        }]),
        Ast::Tuple { exprs, info: _ } => {
            let mut effects = Vec::new();
            for expr in exprs {
                effects.extend(into_effect_ast(expr)?);
            }
            Ok(effects)
        }
        Ast::Block { exprs, info: _ } => {
            // { IO } -> single effect
            // { IO, File } -> block containing a single tuple: Tuple([IO, File])
            // { IO; File } -> block with multiple exprs (semicolon-separated)
            let mut effects = Vec::new();
            for expr in exprs {
                effects.extend(into_effect_ast(expr)?);
            }
            Ok(effects)
        }
        _ => Err(ParseError::new(
            "Expected an effect expression".to_string(),
            effect_expr.info().clone(),
        )),
    }
}

fn try_into_refinement(
    fields: &[(RecordKey, Ast)],
    info: &LineInfo,
) -> Result<Option<TypeAst>, ParseError> {
    if fields.is_empty() {
        return Ok(None);
    }
    // Check if the last field has a `|` refinement
    let last_idx = fields.len() - 1;
    let (last_binder, last_field_ty) = &fields[last_idx];
    let Ast::Binary { lhs, op, rhs, .. } = last_field_ty else {
        return Ok(None);
    };
    if op.symbol != SUM_TYPE_SYM {
        return Ok(None);
    }
    let base_is_type = matches!(into_type_ast((**lhs).clone()), Ok(_));
    let pred_is_type = matches!(into_type_ast((**rhs).clone()), Ok(_));
    if !base_is_type || pred_is_type {
        return Ok(None);
    }

    // Build base type: convert all fields (splitting the last field's `|` part)
    let mut base_fields: Vec<(RecordKey, TypeAst)> = Vec::new();
    for (i, (key, field_ast)) in fields.iter().enumerate() {
        if i == last_idx {
            base_fields.push((key.clone(), into_type_ast((**lhs).clone())?));
        } else {
            base_fields.push((key.clone(), into_type_ast(field_ast.clone())?));
        }
    }

    let base = if base_fields.len() == 1 {
        base_fields.into_iter().next().unwrap().1
    } else {
        TypeAst::Record {
            fields: base_fields,
            info: info.clone(),
        }
    };

    Ok(Some(TypeAst::Refinement {
        binder: last_binder.clone(),
        base: Box::new(base),
        predicate: Box::new((**rhs).clone()),
        info: info.clone(),
    }))
}

fn array_len_from_ast(ast: Ast) -> Result<ArrayLenAst, ParseError> {
    let info = ast.info().clone();
    if let Ast::Identifier { name, .. } = ast.clone() {
        return Ok(ArrayLenAst::Symbol(name));
    }
    let Ast::Literal { value, .. } = ast else {
        return Err(ParseError::new(
            "Expected array length to be an integer literal".to_string(),
            info,
        ));
    };
    let crate::interpreter::value::Value::Number(n) = value else {
        return Err(ParseError::new(
            "Expected array length to be a numeric literal".to_string(),
            info,
        ));
    };
    number_to_usize(&n).map(ArrayLenAst::Known).ok_or_else(|| {
        ParseError::new(
            "Expected array length to be a non-negative integer".to_string(),
            info,
        )
    })
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
