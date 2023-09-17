use crate::{FuncData, Value};

/// Processes a function if it's a built in function. To see a list of built in functions in Yarn, consult
/// (this_page)[<https://docs.yarnspinner.dev/getting-started/writing-in-yarn/functions>]. Additionally, some C# namespaced
/// functions are handled, particularly algebraic and logical functions.
///
/// In practice, because you can define your own functions, you may with to adopt the following pattern:
///
/// ```rs,no_run
/// # fn handle_func(func_data: &FuncData) -> Result<YarnValue, FuncError> {
/// yarn_spinner::process_built_in_functions(func_data).unwrap_or_else(|func_data| {
///     // handle the function yourself here.
/// })
/// # }
/// ```
pub fn process_built_in_function(
    func_data: &FuncData,
    runner: &crate::Runner,
) -> Option<Result<Value, FuncError>> {
    if is_built_in_function(&func_data.function_name) {
        Some(handle_known_default_function(func_data, runner))
    } else {
        None
    }
}

/// Checks if a function, given the name, is a built in function.
/// If this returns `true`, then `process_built_in_functions` will always return `Some`. To that end,
/// you can just run `process_built_in_functions` if that works for you.
pub fn is_built_in_function(func_name: &str) -> bool {
    // we have to check seperately for rand
    if cfg!(feature = "rand") && matches!(func_name, "random" | "random_range" | "dice") {
        return true;
    }

    matches!(
        func_name,
        "visited"
            | "visited_count"
            | "round"
            | "round_places"
            | "floor"
            | "ceil"
            | "inc"
            | "dec"
            | "decimal"
            | "int"
            | "Number.EqualTo"
            | "Number.NotEqualTo"
            | "String.EqualTo"
            | "String.NotEqualTo"
            | "Bool.EqualTo"
            | "Bool.NotEqualTo"
            | "Number.GreaterThan"
            | "Number.LessThan"
            | "Number.GreaterThanOrEqualTo"
            | "Number.LessThanOrEqualTo"
            | "Bool.Or"
            | "Bool.And"
            | "Bool.Xor"
            | "Bool.Not"
            | "Number.Add"
            | "Number.Minus"
            | "Number.Multiply"
            | "Number.Divide"
            | "Number.Modulo"
    )
}

// we use this smaller function with only a `Result` for easier writing (we don't need to wrap everything in `Some`)
fn handle_known_default_function(
    func_data: &FuncData,
    runner: &crate::Runner,
) -> Result<Value, FuncError> {
    fn param_count_is(params: &[Value], expected: usize) -> Result<(), FuncError> {
        if params.len() != expected {
            return Err(FuncError::UnexpectedParamCount {
                expected,
                found: params.len(),
            });
        }

        Ok(())
    }

    fn extract_f32(param: &Value) -> Result<f32, FuncError> {
        match param {
            Value::Bool(_) => Err(FuncError::UnexpectedParamType {
                expected: crate::type_names::F32,
                found: crate::type_names::BOOL,
            }),
            Value::Str(_) => Err(FuncError::UnexpectedParamType {
                expected: crate::type_names::F32,
                found: crate::type_names::STR,
            }),
            Value::F32(v) => Ok(*v),
        }
    }

    fn extract_str(param: &Value) -> Result<&str, FuncError> {
        match param {
            Value::Bool(_) => Err(FuncError::UnexpectedParamType {
                expected: crate::type_names::STR,
                found: crate::type_names::BOOL,
            }),
            Value::Str(v) => Ok(v),
            Value::F32(_) => Err(FuncError::UnexpectedParamType {
                expected: crate::type_names::STR,
                found: crate::type_names::F32,
            }),
        }
    }

    fn extract_bool(param: &Value) -> Result<bool, FuncError> {
        match param {
            Value::Bool(v) => Ok(*v),
            Value::Str(_) => Err(FuncError::UnexpectedParamType {
                expected: crate::type_names::BOOL,
                found: crate::type_names::STR,
            }),
            Value::F32(_) => Err(FuncError::UnexpectedParamType {
                expected: crate::type_names::BOOL,
                found: crate::type_names::F32,
            }),
        }
    }

    match func_data.function_name.as_str() {
        "visited" => {
            param_count_is(&func_data.parameters, 1)?;
            let name = extract_str(&func_data.parameters[0])?;

            Ok(Value::Bool(runner.visited_nodes().contains_key(name)))
        }
        "visited_count" => {
            param_count_is(&func_data.parameters, 1)?;
            let name = extract_str(&func_data.parameters[0])?;

            // as per ys docs, when `visited == false`, `visited_count == 0`.
            Ok(Value::F32(
                runner
                    .visited_nodes()
                    .get(name)
                    .map(|v| *v as f32)
                    .unwrap_or_default(),
            ))
        }
        #[cfg(feature = "rand")]
        "random" => {
            use rand::Rng;
            param_count_is(&func_data.parameters, 0)?;

            Ok(Value::F32(rand::thread_rng().gen_range(0.0..1.0)))
        }
        #[cfg(feature = "rand")]
        "random_range" => {
            use rand::Rng;
            param_count_is(&func_data.parameters, 2)?;

            let bottom = extract_f32(&func_data.parameters[0])?;
            let top = extract_f32(&func_data.parameters[1])?;

            Ok(Value::F32(rand::thread_rng().gen_range(bottom..top)))
        }
        #[cfg(feature = "rand")]
        "dice" => {
            use rand::Rng;
            param_count_is(&func_data.parameters, 1)?;
            let top = extract_f32(&func_data.parameters[0])? as u32;

            Ok(Value::F32(rand::thread_rng().gen_range(1..top) as f32))
        }
        "round" => {
            param_count_is(&func_data.parameters, 1)?;
            let num = extract_f32(&func_data.parameters[0])?;

            Ok(Value::F32(num.round()))
        }
        "round_places" => {
            param_count_is(&func_data.parameters, 2)?;
            let num = extract_f32(&func_data.parameters[0])?;
            let places = extract_f32(&func_data.parameters[1])?;

            let places_f = 10.0f32.powi(places as i32);

            Ok(Value::F32((num * places_f).round() / places_f))
        }
        "floor" => {
            param_count_is(&func_data.parameters, 1)?;
            let num = extract_f32(&func_data.parameters[0])?;

            let output = if num.is_sign_negative() {
                num.ceil()
            } else {
                num.floor()
            };

            Ok(Value::F32(output))
        }
        "ceil" => {
            param_count_is(&func_data.parameters, 1)?;
            let num = extract_f32(&func_data.parameters[0])?;

            let output = if num.is_sign_negative() {
                num.floor()
            } else {
                num.ceil()
            };

            Ok(Value::F32(output))
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

            Ok(Value::F32(output))
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

            Ok(Value::F32(output))
        }
        "decimal" => {
            param_count_is(&func_data.parameters, 1)?;
            let num = extract_f32(&func_data.parameters[0])?;

            Ok(Value::F32(num.fract()))
        }
        "int" => {
            param_count_is(&func_data.parameters, 1)?;
            let num = extract_f32(&func_data.parameters[0])?;

            Ok(Value::F32(num.floor()))
        }
        "Number.EqualTo" => {
            param_count_is(&func_data.parameters, 2)?;
            let a = extract_f32(&func_data.parameters[1])?;
            let b = extract_f32(&func_data.parameters[0])?;

            Ok(Value::Bool(a == b))
        }
        "Number.NotEqualTo" => {
            param_count_is(&func_data.parameters, 2)?;
            let a = extract_f32(&func_data.parameters[1])?;
            let b = extract_f32(&func_data.parameters[0])?;

            Ok(Value::Bool(a != b))
        }
        "String.EqualTo" => {
            param_count_is(&func_data.parameters, 2)?;
            let a = extract_str(&func_data.parameters[1])?;
            let b = extract_str(&func_data.parameters[0])?;

            Ok(Value::Bool(a == b))
        }
        "String.NotEqualTo" => {
            param_count_is(&func_data.parameters, 2)?;
            let a = extract_str(&func_data.parameters[1])?;
            let b = extract_str(&func_data.parameters[0])?;

            Ok(Value::Bool(a != b))
        }
        "Bool.EqualTo" => {
            param_count_is(&func_data.parameters, 2)?;
            let a = extract_bool(&func_data.parameters[1])?;
            let b = extract_bool(&func_data.parameters[0])?;

            Ok(Value::Bool(a == b))
        }
        "Bool.NotEqualTo" => {
            param_count_is(&func_data.parameters, 2)?;
            let a = extract_bool(&func_data.parameters[1])?;
            let b = extract_bool(&func_data.parameters[0])?;

            Ok(Value::Bool(a != b))
        }
        "Number.GreaterThan" => {
            param_count_is(&func_data.parameters, 2)?;
            let a = extract_f32(&func_data.parameters[1])?;
            let b = extract_f32(&func_data.parameters[0])?;

            Ok(Value::Bool(a > b))
        }
        "Number.LessThan" => {
            param_count_is(&func_data.parameters, 2)?;
            let a = extract_f32(&func_data.parameters[1])?;
            let b = extract_f32(&func_data.parameters[0])?;

            Ok(Value::Bool(a < b))
        }
        "Number.GreaterThanOrEqualTo" => {
            param_count_is(&func_data.parameters, 2)?;
            let a = extract_f32(&func_data.parameters[1])?;
            let b = extract_f32(&func_data.parameters[0])?;

            Ok(Value::Bool(a >= b))
        }
        "Number.LessThanOrEqualTo" => {
            param_count_is(&func_data.parameters, 2)?;
            let a = extract_f32(&func_data.parameters[1])?;
            let b = extract_f32(&func_data.parameters[0])?;

            Ok(Value::Bool(a <= b))
        }
        "Bool.Or" => {
            param_count_is(&func_data.parameters, 2)?;
            let a = extract_bool(&func_data.parameters[1])?;
            let b = extract_bool(&func_data.parameters[0])?;

            Ok(Value::Bool(a || b))
        }
        "Bool.And" => {
            param_count_is(&func_data.parameters, 2)?;
            let a = extract_bool(&func_data.parameters[1])?;
            let b = extract_bool(&func_data.parameters[0])?;

            Ok(Value::Bool(a && b))
        }
        "Bool.Xor" => {
            param_count_is(&func_data.parameters, 2)?;
            let a = extract_bool(&func_data.parameters[1])?;
            let b = extract_bool(&func_data.parameters[0])?;

            Ok(Value::Bool(a ^ b))
        }
        "Bool.Not" => {
            param_count_is(&func_data.parameters, 1)?;
            let a = extract_bool(&func_data.parameters[0])?;

            Ok(Value::Bool(!a))
        }
        "Number.Add" => {
            param_count_is(&func_data.parameters, 2)?;
            let a = extract_f32(&func_data.parameters[1])?;
            let b = extract_f32(&func_data.parameters[0])?;

            Ok(Value::F32(a + b))
        }
        "Number.Minus" => {
            param_count_is(&func_data.parameters, 2)?;
            let a = extract_f32(&func_data.parameters[1])?;
            let b = extract_f32(&func_data.parameters[0])?;

            Ok(Value::F32(a - b))
        }
        "Number.Multiply" => {
            param_count_is(&func_data.parameters, 2)?;
            let a = extract_f32(&func_data.parameters[1])?;
            let b = extract_f32(&func_data.parameters[0])?;

            Ok(Value::F32(a * b))
        }
        "Number.Divide" => {
            param_count_is(&func_data.parameters, 2)?;
            let a = extract_f32(&func_data.parameters[1])?;
            let b = extract_f32(&func_data.parameters[0])?;

            Ok(Value::F32(a / b))
        }
        "Number.Modulo" => {
            param_count_is(&func_data.parameters, 2)?;
            let a = extract_f32(&func_data.parameters[1])?;
            let b = extract_f32(&func_data.parameters[0])?;

            Ok(Value::F32(a % b))
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
