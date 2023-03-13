use rand::Rng;

use crate::{FuncData, YarnValue};

/// If this returns `None`, then this function is not a default function (see `is_default_function`),
/// and users must handle this themselves.
pub fn handle_default_functions(func_data: &FuncData) -> Option<Result<YarnValue, FuncError>> {
    if is_default_function(&func_data.function_name) {
        handle_known_default_function(func_data)
    }

    todo!()
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
            return Err(FuncError::ParameterMismatch {
                expected,
                found: params.len(),
            });
        }

        Ok(())
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

            Ok(YarnValue::F32(rand::thread_rng().gen_range(0.0..1.0)))
        }
        "dice" => {
            todo!()
        }
        "round" => {
            todo!()
        }
        "round_places" => {}
        "floor" => {}
        "ceil" => {}
        "inc" => {}
        "dec" => {}
        "decimal" => {}
        "int" => {}
        _ => unreachable!(),
    };
}

#[derive(Debug, thiserror::Error)]
pub enum FuncError {
    #[error("incorrect parameter count: expected {expected} but found {found}")]
    ParameterMismatch { expected: usize, found: usize },
}
