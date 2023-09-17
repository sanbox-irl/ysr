#![doc = include_str!("../README.md")]
#![deny(unsafe_code)]
#![deny(rust_2018_idioms)]
#![deny(missing_docs)]
#![warn(clippy::dbg_macro)]
#![warn(clippy::print_stdout)]
#![warn(clippy::todo)]
#![deny(rustdoc::broken_intra_doc_links)]

use std::collections::HashMap;

mod byte_code;
mod functions;
mod localization;
mod markup;
mod proto;
mod storage;

pub use byte_code::{Program, ProgramError};
pub use functions::{is_built_in_function, process_built_in_function};
pub use localization::Localization;
pub use markup::*;
pub use storage::Storage;

use byte_code::Instruction;

/// A Virtual Machine which executes a [Program].
#[derive(Debug)]
pub struct Runner {
    program: Program,
    visited_nodes: HashMap<String, usize>,

    waiting_status: Option<WaitingStatus>,
    state: Option<State>,
}

#[derive(Debug)]
struct State {
    node_name: String,
    instruction: usize,
    stack: Vec<Value>,
    options: Vec<YarnOption>,
}

impl Runner {
    /// Creates a new [Runner] with the given Program and a node to start on.
    ///
    /// ## Errors
    ///
    /// This errors when `starting_node` does not exist in the program.
    pub fn new(
        program: Program,
        starting_node: impl Into<String>,
    ) -> Result<Self, NodeDoesNotExist> {
        let mut me = Self::without_starting_node(program);

        me.set_node(starting_node).map(|()| me)
    }

    /// Creates a new [Runner] with the given Program and without a starting node.
    /// You'll need to call [set_node](Self::set_node) before execution can start.
    pub fn without_starting_node(program: Program) -> Self {
        Self {
            program,
            visited_nodes: HashMap::new(),
            waiting_status: None,
            state: None,
        }
    }

    /// Get a reference to the inner yarn program
    pub fn program(&self) -> &Program {
        &self.program
    }

    /// Manually sets the node that we run. Returns `Ok` if a node by that name exists in the loaded `Program`.
    ///
    /// ## Errors
    ///
    /// Errors if the node does not exist in the program.
    pub fn set_node(&mut self, node_name: impl Into<String>) -> Result<(), NodeDoesNotExist> {
        let node_name = node_name.into();

        if !self.program.nodes.contains_key(&node_name) {
            return Err(NodeDoesNotExist(node_name));
        }

        let mut old_name = None;

        // reuse the old stack and options, cause why not
        let (stack, options) = self
            .state
            .take()
            .map(|mut old_state| {
                old_state.stack.clear();
                old_state.options.clear();

                // grab the old name...
                old_name = Some(old_state.node_name);

                (old_state.stack, old_state.options)
            })
            .unwrap_or_default();

        // mark that node as having been visited
        if let Some(old_name) = old_name {
            *self.visited_nodes.entry(old_name).or_default() += 1;
        }

        self.state = Some(State {
            node_name,
            instruction: 0,
            stack,
            options,
        });

        self.waiting_status = None;

        Ok(())
    }

    /// Attempts to move the state of the runner forward.
    ///
    /// ## Errors
    ///
    /// Errors if the dialogue runner is not in a valid state (either hasn't has a node set, is waiting for a function or a option to be select),
    /// or internal yarn errors occur, such as trying to jump to a node which doesn't eixst, failing to convert values, unknown variable reads, etc.
    pub fn execute(&mut self, storage: &mut Storage) -> Result<ExecutionOutput, ExecutionError> {
        let Some(mut state) = self.state.as_mut() else {
            return Err(ExecutionError::NotReadyToExecute(
                NotReadyToExecute::NoNodeSelected,
            ));
        };

        match self.waiting_status {
            // the issue is whether we've got a state really
            Some(WaitingStatus::OptionSelection(_)) => {
                return Err(ExecutionError::NotReadyToExecute(
                    NotReadyToExecute::WaitingForOptionSelection,
                ));
            }
            Some(WaitingStatus::FunctionReturn) => {
                return Err(ExecutionError::NotReadyToExecute(
                    NotReadyToExecute::WaitingForFunctionReturn,
                ));
            }
            None => {
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

            let Some(instruction) = node.instructions.get(instruction_count) else {
                break;
            };

            match instruction {
                Instruction::JumpTo(node_name) => {
                    state.instruction = *node
                        .jump_table
                        .get(node_name)
                        .expect("is the compiler wrong here?");
                }
                Instruction::JumpIfFalse(node_name) => {
                    let stack_top = state.stack.last().expect("is this possible?");
                    if !stack_top
                        .try_to_bool()
                        .map_err(ScriptError::ConditionOnNonBool)?
                    {
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

                    return Ok(ExecutionOutput::Line(Line {
                        string_key: line.string_key.clone(),
                        substitutions,
                    }));
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

                    return Ok(ExecutionOutput::Command(command));
                }
                Instruction::AddOption(option_data) => {
                    let mut substitutions = vec![];

                    for _ in 0..option_data.line.substitution_count {
                        substitutions.push(state.stack.pop().expect("handle errr").to_string());
                    }

                    let condition_passed = if option_data.has_condition {
                        let output = state.stack.pop().expect("malformed stack");
                        Some(
                            output
                                .try_to_bool()
                                .map_err(ScriptError::ConditionOnNonBool)?,
                        )
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
                    self.waiting_status = Some(WaitingStatus::OptionSelection(
                        state
                            .options
                            .iter()
                            .map(|v| v.destination.clone())
                            .collect(),
                    ));

                    // give em da options!
                    return Ok(ExecutionOutput::Options(std::mem::take(&mut state.options)));
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
                    let param_count = state
                        .stack
                        .pop()
                        .unwrap()
                        .try_to_f32()
                        .map_err(ScriptError::ConversionError)?
                        as usize;

                    let mut parameters = vec![];

                    for _ in 0..param_count {
                        parameters.push(state.stack.pop().unwrap());
                    }

                    self.waiting_status = Some(WaitingStatus::FunctionReturn);

                    return Ok(ExecutionOutput::Function(FuncData {
                        function_name: function_name.clone(),
                        parameters,
                    }));
                }
                Instruction::PushVar(v) => {
                    let value = match storage.get(v) {
                        Some(v) => v,
                        None => {
                            // okay let's check the default type listings
                            match self.program.initial_values.get(v) {
                                Some(v) => v,
                                None => {
                                    return Err(ExecutionError::ScriptError(
                                        ScriptError::UnknownVariable(v.to_string()),
                                    ))
                                }
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
        Ok(ExecutionOutput::Finished)
    }

    /// Tries to set the option selection.
    ///
    /// ## Errors
    /// - If we are not in WaitingForOptionSelection
    /// - If the selection given is `>= options.len()` (ie, you chose 4 out of 2 options).
    pub fn select_option(&mut self, selection: usize) -> Result<(), OptionSelectionError> {
        let Some(WaitingStatus::OptionSelection(destinations)) = &mut self.waiting_status else {
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
        self.waiting_status = None;

        Ok(())
    }

    /// Returns the output of a function requested by the runner.
    ///
    /// ## Errors
    ///
    /// This errors only when the runner was not expecting a function value to be returned.
    /// Note: we currently DO NOT have a way to know what type of value is expected, so an incorrect
    /// type will result, likely, in a conversion error, or cascading errors, later on.
    pub fn return_function(&mut self, value: Value) -> Result<(), UnexpectedFunctionReturn> {
        let Some(WaitingStatus::FunctionReturn) = &self.waiting_status else {
            return Err(UnexpectedFunctionReturn);
        };

        let state = self.state.as_mut().expect("internal yarn-runner error");
        state.stack.push(value);
        self.waiting_status = None;

        Ok(())
    }

    /// Gets the current [WaitingStatus]. If this isn't [None], then we're waiting on *something*.
    pub fn waiting_status(&self) -> &Option<WaitingStatus> {
        &self.waiting_status
    }

    /// Gets immutable access to the map of visited nodes. This is only used to process the commands `visited` and `visited_count`.
    /// See [crate::process_built_in_function] for more.
    pub fn visited_nodes(&self) -> &HashMap<String, usize> {
        &self.visited_nodes
    }

    /// Gets mutable access to the map of visited nodes. This is only used to process the commands `visited` and `visited_count`.
    /// See [crate::process_built_in_function] for more.
    ///
    /// Generally, you shouldn't mutate this data to accurately reflect the program's progress, but nothing tracks this map, so you could do
    /// whatever you want to it.
    pub fn visited_nodes_mut(&mut self) -> &mut HashMap<String, usize> {
        &mut self.visited_nodes
    }
}

/// The output of the Runner. Contractually, different outputs require different gameplay
/// patterns to occur, though you can always check the [WaitingStatus] with [Runner::waiting_status]
/// to find what the Runner needs (if anything) before [Runner::execute] can be called succesfully again.
#[derive(Debug)]
pub enum ExecutionOutput {
    /// A line to be displayed, though it should be ran through the Markup parser before display. You can run [Runner::execute] again
    /// immediately.
    Line(Line),
    /// Various options which the player can choose from. You must run [Runner::select_option] before [Runner::execute] can be ran again.
    Options(Vec<YarnOption>),
    /// The command string which the player is trying to run. This string is unformatted and unprocessed, so can be anything. You can run
    /// [Runner::execute] again immediately.
    Command(String),
    /// A function and its parameters, already parsed lightly be the Runner. You must run [Runner::return_function] before [Runner::execute] can be ran again.
    Function(FuncData),
    /// The program is finished. You must run [Runner::set_node] before [Runner::execute] can be ran again.
    Finished,
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
pub enum WaitingStatus {
    /// The [Runner] is waiting for an option to be selected with `select_option`.
    OptionSelection(Vec<String>),

    /// The [Runner] is waiting for a function dispatch to return with `return_function`
    FunctionReturn,
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
    ///
    /// ## Errors
    ///
    /// Returns a [ConversionError] when a string which is not `true`, `TRUE`, `false`, or `FALSE` is passed.
    /// All other types do not have any error cases.
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
    ///
    /// ## Errors
    ///
    /// Returns an error when `self` is a str which cannot be parsed via the std's str-f32 parsing.
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

/// This is a Line given out by the Runner. You should hand this line over to [Localization::line_display_text]
/// to convert this to text to show users.
///
/// In general, these fields are made public since editing them is risk-free, but users don't need to read these.
// this is identical to `Line` in `bytecode` but I've made them separate types so
// we have more flexibility with semver.
#[derive(Debug)]
pub struct Line {
    /// The localization key id for the line.
    pub string_key: String,
    /// The subsitutions, if any, used in the line.
    pub substitutions: Vec<String>,
}

/// This is a dialogue option. You should use [line](Self::line) to get a [Line] to show.
#[derive(Debug)]
pub struct YarnOption {
    pub(crate) line: Line,
    pub(crate) destination: String,
    pub(crate) condition_passed: Option<bool>,
}

impl YarnOption {
    /// This is a Line given out by the Runner. You should hand this line over to [Localization::line_display_text]
    /// to convert this to text to show users.
    pub fn line(&self) -> &Line {
        &self.line
    }

    /// Interpret the output as follows:
    /// - `Some(true)`: conditions existed and were passed.
    /// - `Some(false)`: conditions existed and were failed.
    /// - `None`: conditions did not exist.
    pub fn condition_passed(&self) -> Option<bool> {
        self.condition_passed
    }
}

/// Applies substitutions to a given line as needed. This is used internally but given as a utility for users
/// if they want.
pub fn apply_arguments_in_substition(f_string: &str, subs: &[String]) -> String {
    let mut output = f_string.to_owned();

    for (i, sub) in subs.iter().enumerate() {
        output = output.replace(&format!("{{{i}}}"), sub);
    }

    output
}

/// An execution error in the output of the Runner.
#[derive(Debug, thiserror::Error)]
pub enum ExecutionError {
    /// The runner is waiting on something before it can execute.
    #[error(transparent)]
    NotReadyToExecute(NotReadyToExecute),

    /// We had an error in the script itself.
    #[error(transparent)]
    ScriptError(#[from] ScriptError),
}

/// This class of error occurs when a script does something illegal which the ysc could not catch
/// at compile time.
#[derive(Debug, thiserror::Error)]
pub enum ScriptError {
    /// Tried to jump to a node which doesn't exist.
    #[error(transparent)]
    NodeDoesNotExist(NodeDoesNotExist),

    /// Conditions requires variable to be a bool, but it wasn't.
    #[error("condition requires variable to be bool, but it is not: {0}")]
    ConditionOnNonBool(ConversionError),

    /// We couldn't convert between two given types.
    #[error(transparent)]
    ConversionError(ConversionError),

    /// An unknown variable was used.
    #[error("unknown variable binding `{0}`")]
    UnknownVariable(String),
}

impl From<NodeDoesNotExist> for ExecutionError {
    fn from(value: NodeDoesNotExist) -> Self {
        Self::ScriptError(ScriptError::NodeDoesNotExist(value))
    }
}

/// A node does not exist of that name.
#[derive(Debug, thiserror::Error)]
#[error("A node of name {0} is not present in the currently loaded program.")]
pub struct NodeDoesNotExist(String);

/// The runner wasn't ready to be called.
#[derive(Debug, thiserror::Error)]
pub enum NotReadyToExecute {
    /// The runner needs [select_option](Runner::select_option) to be called.
    #[error("waiting for option to be selected. call `select_option`")]
    WaitingForOptionSelection,
    /// The runner needs [return_function](Runner::return_function) to be called.
    #[error("waiting for a function to return a value. call `return_function`")]
    WaitingForFunctionReturn,
    /// No node was selected at all to start or `set_node` was not called.
    #[error("no node selected. call `set_node`")]
    NoNodeSelected,
}

/// We couldn't convert between two types.
#[derive(Debug, thiserror::Error)]
#[error("value of type `{target_type}` could not be converted to `{found_type}`")]
pub struct ConversionError {
    /// The type we wanted to convert to.
    pub target_type: &'static str,
    /// The type we found but could not convert to [target_type](Self::target_type).
    pub found_type: &'static str,
}

/// Attempted to select an option incorrectly.
#[derive(Debug, thiserror::Error)]
pub enum OptionSelectionError {
    /// The runner is not in state `WaitingForOptionSelection`, so this shouldn't have been called.
    #[error("runner is not in state `WaitingForOptionSelection`")]
    UnexpectedOptionSelection,

    /// An option was set that is too high.
    #[error("invalid selection: {input} but len was {count}")]
    OptionSelectionTooHigh {
        /// The inputted sized.
        input: usize,
        /// The max size.
        count: usize,
    },
}

/// The runner is not in state `WaitingForFunctionReturn`, so this shouldn't have been called.
#[derive(Debug, thiserror::Error)]
#[error("runner is not in state `WaitingForFunctionReturn")]
pub struct UnexpectedFunctionReturn;

mod type_names {
    pub const F32: &str = "f32";
    pub const STR: &str = "str";
    pub const BOOL: &str = "bool";
}
