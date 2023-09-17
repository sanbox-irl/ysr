use std::collections::HashMap;

use crate::{type_names, Value};

/// This is the compiled output of the `ysc` compiler, lightly processed by us.
/// Fundamentally, this is the bytecode stream of a yarn program, which the `Runner`
/// can take and actually execute.
#[derive(Debug, Clone)]
pub struct Program {
    pub(crate) name: String,
    pub(crate) nodes: HashMap<String, Node>,
    pub(crate) initial_values: HashMap<String, Value>,
}

impl Program {
    /// Parses in a bytecode stream. This should be the bytecode outputted by `ysc`.
    pub fn new(program: &[u8]) -> Result<Self, ProgramError> {
        // these are all very annoying traits to have outside this scope, so we leave them here.
        use crate::proto::{instruction, operand::Value, Program};

        struct OperandHandler<T>(T, usize);

        impl<T: Iterator<Item = Value>> OperandHandler<T> {
            pub fn take_str(&mut self) -> Result<String, ProgramError> {
                const EXPECTED: &str = type_names::STR;

                match self.0.next() {
                    Some(Value::String(str)) => Ok(str),
                    Some(Value::Bool(_)) => {
                        Err(ProgramError::UnexpectedOperandKind(UnexpectedOperandKind {
                            idx: self.1,
                            expected: EXPECTED,
                            found: type_names::BOOL,
                        }))
                    }
                    Some(Value::Float(_)) => {
                        Err(ProgramError::UnexpectedOperandKind(UnexpectedOperandKind {
                            idx: self.1,
                            expected: EXPECTED,
                            found: type_names::F32,
                        }))
                    }
                    None => Err(ProgramError::MissingOperand {
                        idx: self.1,
                        expected: EXPECTED,
                    }),
                }
            }

            fn take_f32(&mut self) -> Result<f32, ProgramError> {
                const EXPECTED: &str = type_names::F32;

                match self.0.next() {
                    Some(Value::Float(v)) => Ok(v),
                    Some(Value::String(_)) => {
                        Err(ProgramError::UnexpectedOperandKind(UnexpectedOperandKind {
                            idx: self.1,
                            expected: EXPECTED,
                            found: type_names::STR,
                        }))
                    }
                    Some(Value::Bool(_)) => {
                        Err(ProgramError::UnexpectedOperandKind(UnexpectedOperandKind {
                            idx: self.1,
                            expected: EXPECTED,
                            found: type_names::BOOL,
                        }))
                    }
                    None => Err(ProgramError::MissingOperand {
                        idx: self.1,
                        expected: EXPECTED,
                    }),
                }
            }

            fn take_bool(&mut self) -> Result<bool, ProgramError> {
                const EXPECTED: &str = type_names::BOOL;

                match self.0.next() {
                    Some(Value::Bool(v)) => Ok(v),
                    Some(Value::String(_)) => {
                        Err(ProgramError::UnexpectedOperandKind(UnexpectedOperandKind {
                            idx: self.1,
                            expected: EXPECTED,
                            found: type_names::STR,
                        }))
                    }
                    Some(Value::Float(_)) => {
                        Err(ProgramError::UnexpectedOperandKind(UnexpectedOperandKind {
                            idx: self.1,
                            expected: EXPECTED,
                            found: type_names::F32,
                        }))
                    }
                    None => Err(ProgramError::MissingOperand {
                        idx: self.1,
                        expected: EXPECTED,
                    }),
                }
            }

            fn take_floaty_usize(&mut self) -> Result<usize, ProgramError> {
                self.take_f32().map(|v| v as usize)
            }
        }

        // we use this horrible syntax because importing `prost::Message` makes autocomplete much worse in this file.
        let program = match <Program as prost::Message>::decode(program) {
            Ok(v) => v,
            Err(e) => {
                return Err(ProgramError::DecodeErr(e));
            }
        };

        let mut output_nodes = HashMap::new();

        for (node_name, node) in program.nodes {
            // first up, let's convert the instructions
            let mut instructions = vec![];
            for (i, raw_instruction) in node.instructions.into_iter().enumerate() {
                let Some(opcode) = instruction::OpCode::from_i32(raw_instruction.opcode) else {
                    return Err(ProgramError::UnexpectedOpCode {
                        found: raw_instruction.opcode,
                        idx: i,
                    });
                };
                let mut operands = OperandHandler(
                    raw_instruction
                        .operands
                        .into_iter()
                        .map(|v| v.value.expect("operand was null")),
                    i,
                );

                let instruction = match opcode {
                    instruction::OpCode::JumpTo => Instruction::JumpTo(operands.take_str()?),
                    instruction::OpCode::Jump => Instruction::Jump,
                    instruction::OpCode::RunLine => Instruction::RunLine(Line {
                        string_key: operands.take_str()?,
                        substitution_count: operands.take_floaty_usize()?,
                    }),
                    instruction::OpCode::RunCommand => Instruction::Command(Command {
                        command_string: operands.take_str()?,
                        substitution_count: operands.take_floaty_usize()?,
                    }),
                    instruction::OpCode::AddOption => {
                        // the order of operands is a little odd, so we've got to be explicit here...
                        let string_key = operands.take_str()?;
                        let destination = operands.take_str()?;
                        let substitution_count = operands.take_floaty_usize()?;
                        let has_condition = operands.take_bool()?;

                        Instruction::AddOption(OptionData {
                            line: Line {
                                string_key,
                                substitution_count,
                            },
                            destination,
                            has_condition,
                        })
                    }
                    instruction::OpCode::ShowOptions => Instruction::ShowOptions,
                    instruction::OpCode::PushString => {
                        Instruction::PushString(operands.take_str()?)
                    }
                    instruction::OpCode::PushFloat => Instruction::PushFloat(operands.take_f32()?),
                    instruction::OpCode::PushBool => Instruction::PushBool(operands.take_bool()?),
                    instruction::OpCode::PushNull => {
                        return Err(ProgramError::UnsupportedOpCode(raw_instruction.opcode));
                    }
                    instruction::OpCode::JumpIfFalse => {
                        Instruction::JumpIfFalse(operands.take_str()?)
                    }
                    instruction::OpCode::Pop => Instruction::Pop,
                    instruction::OpCode::CallFunc => Instruction::CallFunc(operands.take_str()?),
                    instruction::OpCode::PushVariable => Instruction::PushVar(operands.take_str()?),
                    instruction::OpCode::StoreVariable => {
                        Instruction::StoreVar(operands.take_str()?)
                    }
                    instruction::OpCode::Stop => Instruction::Stop,
                    instruction::OpCode::RunNode => Instruction::RunNode,
                };

                instructions.push(instruction);
            }

            // we'll need to recollect the whole thing as a usize
            // this is wasteful memory wise but probably nicer for the program overall
            let jump_table = node
                .labels
                .into_iter()
                .map(|(k, v)| (k, v as usize))
                .collect();

            output_nodes.insert(
                node_name,
                Node {
                    instructions,
                    jump_table,
                    tags: node.tags,
                },
            );
        }

        let initial_values = program
            .initial_values
            .into_iter()
            .filter_map(|(k, v)| {
                v.value.map(|v| {
                    let value = match v {
                        Value::String(v) => crate::Value::Str(v),
                        Value::Bool(v) => crate::Value::Bool(v),
                        Value::Float(v) => crate::Value::F32(v),
                    };

                    (k, value)
                })
            })
            .collect();

        Ok(Self {
            name: program.name,
            nodes: output_nodes,
            initial_values,
        })
    }

    /// The name of the program.
    pub fn name(&self) -> &str {
        self.name.as_ref()
    }
}

/// An error reading a program's bytecode stream.
#[derive(Debug, thiserror::Error)]
pub enum ProgramError {
    /// An unexpected op code was found.
    #[error("unexpected opcode `{found}` at index `{idx}`")]
    UnexpectedOpCode {
        /// The unexpected upcode.
        found: i32,
        /// The index in the bytestream.
        idx: usize,
    },

    /// We couldn't decode the bytestream Protobuf at all. This might mean a version incompatibility between this library and the ysc.
    #[error(transparent)]
    DecodeErr(prost::DecodeError),

    /// Occurs only with ysc v1.0.
    #[error(
        "op-code {0} is no longer supported. please recompile your source with a newer compiler."
    )]
    UnsupportedOpCode(i32),

    /// Unexpected operand kind found.
    #[error(transparent)]
    UnexpectedOperandKind(UnexpectedOperandKind),

    /// Missing an operand at a given index.
    #[error("missing operand kind at index `{idx}`: expected `{expected}`")]
    MissingOperand {
        /// The index of the missing operand
        idx: usize,
        /// The name of the operand expected.
        expected: &'static str,
    },
}

#[derive(Debug, thiserror::Error)]
#[error("unexpected operand kind at index `{idx}`: expected `{expected}`, found `{found}`")]
pub struct UnexpectedOperandKind {
    idx: usize,
    expected: &'static str,
    found: &'static str,
}

#[derive(Debug, Clone, PartialEq)]
pub(crate) struct Node {
    /// The core instruction set we actually execute
    pub(crate) instructions: Vec<Instruction>,
    /// A jump table, mapping the names of labels to positions in the
    /// instructions list.
    pub(crate) jump_table: HashMap<String, usize>,
    /// The tags associated with this node.
    pub(crate) tags: Vec<String>,
}

#[derive(Debug, Clone, PartialEq)]
pub(crate) enum Instruction {
    JumpTo(String),
    JumpIfFalse(String),
    Jump,
    RunLine(Line),
    Command(Command),
    AddOption(OptionData),
    ShowOptions,
    PushString(String),
    PushFloat(f32),
    PushBool(bool),
    Pop,
    CallFunc(String),
    PushVar(String),
    StoreVar(String),
    Stop,
    RunNode,
}

#[derive(Debug, Clone, PartialEq)]
pub(crate) struct Line {
    pub(crate) string_key: String,
    pub(crate) substitution_count: usize,
}

#[derive(Debug, Clone, PartialEq)]
pub(crate) struct Command {
    pub(crate) command_string: String,
    pub(crate) substitution_count: usize,
}

#[derive(Debug, Clone, PartialEq)]
pub(crate) struct OptionData {
    pub(crate) line: Line,
    pub(crate) destination: String,
    pub(crate) has_condition: bool,
}
