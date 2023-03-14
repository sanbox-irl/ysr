use rand::Rng;

use crate::{FuncData, YarnValue};

/// If this returns `None`, then this function is not a default function (see `is_default_function`),
/// and users must handle this themselves.
pub fn handle_default_functions(func_data: &FuncData) -> Option<Result<YarnValue, FuncError>> {
    if is_default_function(&func_data.function_name) {
        Some(handle_known_default_function(func_data))
    } else {
        None
    }
}

pub fn is_default_function(func_name: &str) -> bool {
    matches!(
        func_name,
        "visited"
            | "visited_count"
            | "random"
            | "random_range"
            | "dice"
            | "round"
            | "round_places"
            | "floor"
            | "ceil"
            | "inc"
            | "dec"
            | "decimal"
            | "int"
    )
}

// we use this smaller function with only a `Result` for easier writing (we don't need to wrap everything in `Some`)
fn handle_known_default_function(func_data: &FuncData) -> Result<YarnValue, FuncError> {
    fn param_count_is(params: &[YarnValue], expected: usize) -> Result<(), FuncError> {
        if params.len() != expected {
            return Err(FuncError::UnexpectedParamCount {
                expected,
                found: params.len(),
            });
        }

        Ok(())
    }

    fn extract_f32(param: &YarnValue) -> Result<f32, FuncError> {
        match param {
            YarnValue::Bool(_) => Err(FuncError::UnexpectedParamType {
                expected: crate::type_names::F32,
                found: crate::type_names::BOOL,
            }),
            YarnValue::Str(_) => Err(FuncError::UnexpectedParamType {
                expected: crate::type_names::F32,
                found: crate::type_names::STR,
            }),
            YarnValue::F32(v) => Ok(*v),
        }
    }

    match func_data.function_name.as_str() {
        "visited" => {
            todo!()
        }
        "visited_count" => {
            todo!()
        }
        "random" => {
            param_count_is(&func_data.parameters, 0)?;

            Ok(YarnValue::F32(rand::thread_rng().gen_range(0.0..1.0)))
        }
        "random_range" => {
            param_count_is(&func_data.parameters, 2)?;

            let bottom = extract_f32(&func_data.parameters[0])?;
            let top = extract_f32(&func_data.parameters[1])?;

            Ok(YarnValue::F32(rand::thread_rng().gen_range(bottom..top)))
        }
        "dice" => {
            param_count_is(&func_data.parameters, 1)?;
            let top = extract_f32(&func_data.parameters[0])? as u32;

            Ok(YarnValue::F32(rand::thread_rng().gen_range(1..top) as f32))
        }
        "round" => {
            param_count_is(&func_data.parameters, 1)?;
            let num = extract_f32(&func_data.parameters[0])?;

            Ok(YarnValue::F32(num.round()))
        }
        "round_places" => {
            param_count_is(&func_data.parameters, 2)?;
            let num = extract_f32(&func_data.parameters[0])?;
            let places = extract_f32(&func_data.parameters[1])?;

            let places_f = 10.0f32.powi(places as i32);

            Ok(YarnValue::F32((num * places_f).round() / places_f))
        }
        "floor" => {
            param_count_is(&func_data.parameters, 1)?;
            let num = extract_f32(&func_data.parameters[0])?;

            let output = if num.is_sign_negative() {
                num.ceil()
            } else {
                num.floor()
            };

            Ok(YarnValue::F32(output))
        }
        "ceil" => {
            param_count_is(&func_data.parameters, 1)?;
            let num = extract_f32(&func_data.parameters[0])?;

            let output = if num.is_sign_negative() {
                num.floor()
            } else {
                num.ceil()
            };

            Ok(YarnValue::F32(output))
        }
        "inc" => {
            param_count_is(&func_data.parameters, 1)?;
            let num = extract_f32(&func_data.parameters[0])?;

            let output = if num.fract() < f32::EPSILON {
                num + 1.0
            } else if num.is_sign_negative() {
                num.floor()
            } else {
                num.ceil()
            };

            Ok(YarnValue::F32(output))
        }
        "dec" => {
            param_count_is(&func_data.parameters, 1)?;
            let num = extract_f32(&func_data.parameters[0])?;

            let output = if num.fract() < f32::EPSILON {
                num - 1.0
            } else if num.is_sign_negative() {
                num.ceil()
            } else {
                num.floor()
            };

            Ok(YarnValue::F32(output))
        }
        "decimal" => {
            param_count_is(&func_data.parameters, 1)?;
            let num = extract_f32(&func_data.parameters[0])?;

            Ok(YarnValue::F32(num.fract()))
        }
        "int" => {
            param_count_is(&func_data.parameters, 1)?;
            let num = extract_f32(&func_data.parameters[0])?;

            Ok(YarnValue::F32(num.floor()))
        }
        _ => unreachable!(),
    }
}

#[derive(Debug, thiserror::Error)]
pub enum FuncError {
    #[error("unexpected parameter count: expected {expected} but found {found}")]
    UnexpectedParamCount { expected: usize, found: usize },

    #[error("unexpected parameter type: expected {expected} but found {found}")]
    UnexpectedParamType {
        expected: &'static str,
        found: &'static str,
    },
}
