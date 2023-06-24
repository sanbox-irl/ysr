#![doc = include_str!("../README.md")]
#![deny(unsafe_code)]
#![deny(rust_2018_idioms)]
// #![deny(missing_docs)]
#![warn(clippy::dbg_macro)]
#![warn(clippy::print_stdout)]
// #![deny(clippy::missing_errors_doc)]
// #![warn(clippy::missing_panics_doc)]
#![warn(clippy::todo)]
#![deny(rustdoc::broken_intra_doc_links)]

mod byte_code;
mod command_handler;
mod localization;
mod markup;
mod proto;
mod storage;

pub use byte_code::{Program, ProgramError};
pub use command_handler::{is_built_in_function, process_built_in_function};
pub use localization::Localization;
pub use markup::Markup;
pub use storage::Storage;

/// A Virtual Machine which executes a [Program].
#[derive(Debug)]
pub struct Runner {
    program: Program,
    runner_state: RunnerState,

    state: Option<State>,
}

impl Runner {
    /// Creates a new [Runner] with the given Program.
    pub fn new(program: Program) -> Self {
        Self {
            program,
            runner_state: RunnerState::Ready,
            state: None,
        }
    }

    /// Get a reference to the inner yarn program
    pub fn program(&self) -> &Program {
        &self.program
    }

    /// Manually sets the node that we run. Returns `Ok` if a node by that name exists in the loaded `Program`.
    pub fn set_node(&mut self, node_name: impl Into<String>) -> Result<(), NodeDoesNotExist> {
        let node_name = node_name.into();

        if !self.program.nodes.contains_key(&node_name) {
            return Err(NodeDoesNotExist(node_name));
        }

        // perf: we could reuse the stack + options in some manner
        self.state = Some(State {
            node_name,
            instruction: 0,
            stack: vec![],
            options: vec![],
        });

        self.runner_state = RunnerState::Ready;

        Ok(())
    }

    pub fn execute(
        &mut self,
        storage: &mut Storage,
    ) -> Result<Option<ExecutionOutput>, ExecutionError> {
        let Some(mut state) = self.state.as_mut() else {
            return Err(ExecutionError::NotReadyToExecute(NotReadyToExecute::NoNodeSelected));
        };

        match self.runner_state {
            // the issue is whether we've got a state really
            RunnerState::WaitingForOptionSelection(_) => {
                return Err(ExecutionError::NotReadyToExecute(
                    NotReadyToExecute::WaitingForOptionSelection,
                ));
            }
            RunnerState::WaitingForFunctionReturn => {
                return Err(ExecutionError::NotReadyToExecute(
                    NotReadyToExecute::WaitingForFunctionReturn,
                ));
            }
            RunnerState::Ready => {
                // good to go here!
            }
        }

        let mut node = self
            .program
            .nodes
            .get(&state.node_name)
            .expect("we already checked that this was valid");

        loop {
            // save old instruction counter and increment it
            let instruction_count = state.instruction;
            state.instruction += 1;

            let Some(instruction) = node.instructions.get(instruction_count) else { break };

            match instruction {
                Instruction::JumpTo(node_name) => {
                    state.instruction = *node
                        .jump_table
                        .get(node_name)
                        .expect("is the compiler wrong here?");
                }
                Instruction::JumpIfFalse(node_name) => {
                    let stack_top = state.stack.last().expect("is this possible?");
                    if !stack_top.try_to_bool()? {
                        state.instruction = *node
                            .jump_table
                            .get(node_name)
                            .expect("is the compiler wrong here?");
                    }
                }
                Instruction::Jump => {
                    state.instruction = *node
                        .jump_table
                        .get(&state.stack.last().expect("must handle").to_string())
                        .expect("is the compiler wrong here?");
                }
                Instruction::RunLine(line) => {
                    let mut substitutions = vec![];
                    for _ in 0..line.substitution_count {
                        substitutions.push(state.stack.pop().expect("handle errr").to_string());
                    }

                    return Ok(Some(ExecutionOutput::Line(Line {
                        string_key: line.string_key.clone(),
                        substitutions,
                    })));
                }
                Instruction::Command(command_datas) => {
                    let mut substitutions = vec![];
                    for _ in 0..command_datas.substitution_count {
                        substitutions.push(state.stack.pop().expect("handle errr").to_string());
                    }

                    let command = apply_arguments_in_substition(
                        &command_datas.command_string,
                        &substitutions,
                    );

                    return Ok(Some(ExecutionOutput::Command(command)));
                }
                Instruction::AddOption(option_data) => {
                    let mut substitutions = vec![];

                    for _ in 0..option_data.line.substitution_count {
                        substitutions.push(state.stack.pop().expect("handle errr").to_string());
                    }

                    let condition_passed = if option_data.has_condition {
                        let output = state.stack.pop().expect("malformed stack");
                        match output.try_to_bool() {
                            Ok(v) => Some(v),
                            Err(e) => {
                                return Err(ExecutionError::ConditionOnNonBool(e));
                            }
                        }
                    } else {
                        None
                    };

                    state.options.push(YarnOption {
                        line: Line {
                            string_key: option_data.line.string_key.clone(),
                            substitutions,
                        },
                        destination: option_data.destination.clone(),
                        condition_passed,
                    });
                }
                Instruction::ShowOptions => {
                    // switch our state over...
                    self.runner_state = RunnerState::WaitingForOptionSelection(
                        state
                            .options
                            .iter()
                            .map(|v| v.destination.clone())
                            .collect(),
                    );

                    // give em da options!
                    return Ok(Some(ExecutionOutput::Options(std::mem::take(
                        &mut state.options,
                    ))));
                }
                Instruction::PushString(str) => {
                    state.stack.push(Value::Str(str.clone()));
                }
                Instruction::PushFloat(f) => {
                    state.stack.push(Value::F32(*f));
                }
                Instruction::PushBool(b) => {
                    state.stack.push(Value::Bool(*b));
                }
                Instruction::Pop => {
                    state.stack.pop();
                }
                Instruction::CallFunc(function_name) => {
                    let param_count = state.stack.pop().unwrap().try_to_f32()? as usize;

                    let mut parameters = vec![];

                    for _ in 0..param_count {
                        parameters.push(state.stack.pop().unwrap());
                    }

                    self.runner_state = RunnerState::WaitingForFunctionReturn;

                    return Ok(Some(ExecutionOutput::Function(FuncData {
                        function_name: function_name.clone(),
                        parameters,
                    })));
                }
                Instruction::PushVar(v) => {
                    let value = match storage.get(v) {
                        Some(v) => v,
                        None => {
                            // okay let's check the default type listings
                            match self.program.initial_values.get(v) {
                                Some(v) => v,
                                None => return Err(ExecutionError::UnknownVariable(v.to_string())),
                            }
                        }
                    };

                    state.stack.push(value.clone());
                }
                Instruction::StoreVar(value_name) => {
                    let value = state.stack.last().expect("must handle").clone();

                    storage.insert(value_name.clone(), value);
                }
                Instruction::Stop => {
                    self.state = None;

                    break;
                }
                Instruction::RunNode => {
                    let node_name = state.stack.pop().expect("must handle").to_string();

                    // node completion handle?
                    self.set_node(node_name)?;

                    state = self.state.as_mut().unwrap();
                    node = self
                        .program
                        .nodes
                        .get(&state.node_name)
                        .expect("we already checked that this was valid");
                }
            }
        }

        // This also means we reached the end of our program's execution!
        Ok(None)
    }

    /// Tries to set the option selection.
    ///
    /// ## Errors
    /// - If we are not in WaitingForOptionSelection
    /// - If the selection given is `>= options.len()` (ie, you chose 4 out of 2 options).
    pub fn select_option(&mut self, selection: usize) -> Result<(), OptionSelectionError> {
        let RunnerState::WaitingForOptionSelection(destinations) = &mut self.runner_state else {
            return Err(OptionSelectionError::UnexpectedOptionSelection);
        };

        let Some(destination_name) = destinations.get(selection) else {
            return Err(OptionSelectionError::OptionSelectionTooHigh {
                input: selection,
                count: destinations.len(),
            });
        };

        let state = self.state.as_mut().expect("internal yarn-runner error");
        state.stack.push(Value::Str(destination_name.to_owned()));
        self.runner_state = RunnerState::Ready;

        Ok(())
    }

    pub fn return_function(&mut self, value: Value) -> Result<(), UnexpectedFunctionReturn> {
        if self.runner_state != RunnerState::WaitingForFunctionReturn {
            return Err(UnexpectedFunctionReturn);
        };

        let state = self.state.as_mut().expect("internal yarn-runner error");
        state.stack.push(value);
        self.runner_state = RunnerState::Ready;

        Ok(())
    }
}

#[derive(Debug)]
struct State {
    node_name: String,
    instruction: usize,
    stack: Vec<Value>,
    options: Vec<YarnOption>,
}

#[derive(Debug)]
pub enum ExecutionOutput {
    Line(Line),
    Options(Vec<YarnOption>),
    Command(String),
    Function(FuncData),
}

/// The data of a function call, such as `<< if roll(6) >>`
#[derive(Debug)]
pub struct FuncData {
    /// The function name.
    ///
    /// In the example `roll(6)`, this would be `"roll"`.
    pub function_name: String,
    /// The parameters given by the user. This is not necessarily the correct amount of parameters.
    ///
    /// In the example `roll(6)`, this would be `vec![YarnValue::F32(6.0)]`
    pub parameters: Vec<Value>,
}

/// This represents the current state of the [Runner].
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone)]
pub enum RunnerState {
    /// The [Runner] is ready for `execute` to be called.
    Ready,

    /// The [Runner] is waiting for an option to be selected with `select_option`.
    WaitingForOptionSelection(Vec<String>),

    /// The [Runner] is waiting for a function dispatch to return.
    WaitingForFunctionReturn,
}

/// Yarn supports three kinds of values: f32s, bools, and Strings.
#[derive(Debug, PartialEq, PartialOrd, Clone)]
pub enum Value {
    /// A boolean
    Bool(bool),
    /// An owned string
    Str(String),
    /// An f32
    F32(f32),
}

impl std::fmt::Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Bool(v) => v.fmt(f),
            Value::Str(v) => v.fmt(f),
            Value::F32(v) => v.fmt(f),
        }
    }
}

impl Value {
    /// Converts a given value to a bool.
    pub fn try_to_bool(&self) -> Result<bool, ConversionError> {
        let v = match self {
            Value::Bool(v) => *v,
            Value::Str(s) => {
                // these rules need to be taken from the C# source
                match s.as_str() {
                    // the C# implementation actually supports arbitrary capitalization
                    // but we're not supporting out for the sake of my typing
                    "true" | "True" => true,
                    "false" | "FALSE" => false,
                    _ => {
                        return Err(ConversionError {
                            target_type: type_names::BOOL,
                            found_type: type_names::STR,
                        })
                    }
                }
            }
            Value::F32(f) => *f != 0.0,
        };

        Ok(v)
    }

    /// Converts a given value to a string.
    pub fn try_to_f32(&self) -> Result<f32, ConversionError> {
        let v = match self {
            Value::Bool(v) => {
                if *v {
                    1.0
                } else {
                    0.0
                }
            }
            Value::Str(s) => s.parse::<f32>().map_err(|_| ConversionError {
                target_type: type_names::F32,
                found_type: type_names::STR,
            })?,
            Value::F32(f) => *f,
        };

        Ok(v)
    }
}

// this is identical to `Line` in `bytecode` but I've made them separate types so
// we have more flexibility with semver.
#[derive(Debug)]
pub struct Line {
    pub(crate) string_key: String,
    pub(crate) substitutions: Vec<String>,
}

#[derive(Debug)]
pub struct YarnOption {
    pub(crate) line: Line,
    pub(crate) destination: String,

    /// if `None`, then this line never had a condition. If Some, the status is if it passed.
    pub(crate) condition_passed: Option<bool>,
}

impl YarnOption {
    pub fn line(&self) -> &Line {
        &self.line
    }

    /// Returns an option if there is a condition at all.
    pub fn condition_passed(&self) -> Option<bool> {
        self.condition_passed
    }
}

/// Applies substitutions to a given line as needed.
pub fn apply_arguments_in_substition(f_string: &str, subs: &[String]) -> String {
    let mut output = f_string.to_owned();

    for (i, sub) in subs.iter().enumerate() {
        output = output.replace(&format!("{{{i}}}"), sub);
    }

    output
}

#[derive(Debug, thiserror::Error)]
pub enum ExecutionError {
    #[error(transparent)]
    NotReadyToExecute(NotReadyToExecute),

    #[error(transparent)]
    NodeDoesNotExist(#[from] NodeDoesNotExist),

    #[error("condition requires variable to be bool, but it is not: {0}")]
    ConditionOnNonBool(ConversionError),

    #[error(transparent)]
    ConversionError(#[from] ConversionError),

    #[error("unknown variable binding `{0}`")]
    UnknownVariable(String),
}

#[derive(Debug, thiserror::Error)]
#[error("A node of name {0} is not present in the currently loaded program.")]
pub struct NodeDoesNotExist(String);

#[derive(Debug, thiserror::Error)]
pub enum NotReadyToExecute {
    #[error("waiting for option to be selected. call `select_option`")]
    WaitingForOptionSelection,
    #[error("waiting for a function to return a value. call `return_function`")]
    WaitingForFunctionReturn,
    #[error("no node selected. call `set_node`")]
    NoNodeSelected,
}

#[derive(Debug, thiserror::Error)]
#[error("value of type `{target_type}` could not be converted to `{found_type}`")]
pub struct ConversionError {
    target_type: &'static str,
    found_type: &'static str,
}

#[derive(Debug, thiserror::Error)]
pub enum OptionSelectionError {
    #[error("runner is not in state `WaitingForOptionSelection`")]
    UnexpectedOptionSelection,
    #[error("invalid selection: {input} but len was {count}")]
    OptionSelectionTooHigh { input: usize, count: usize },
}

#[derive(Debug, thiserror::Error)]
#[error("runner is not in state `WaitingForFunctionReturn")]
pub struct UnexpectedFunctionReturn;

use byte_code::Instruction;
// utility
mod type_names {
    pub const F32: &str = "f32";
    pub const STR: &str = "str";
    pub const BOOL: &str = "bool";
}
