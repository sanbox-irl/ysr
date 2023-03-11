use std::collections::HashMap;

use crate::YarnValue;

/// This is the compiled output of the `ysc` compiler, lightly processed by us.
/// Fundamentally, this is the bytecode stream of a yarn program, which the `YarnRunner`
/// can take and actually execute.
#[derive(Debug)]
pub struct YarnProgram {
    pub(crate) name: String,
    pub(crate) nodes: HashMap<String, Node>,
    pub(crate) initial_values: HashMap<String, YarnValue>,
}

impl YarnProgram {
    /// Parses in a bytecode stream. This should be the bytecode outputted by `ysc`.
    pub fn new(program: &[u8]) -> Result<Self, YarnProgramError> {
        // these are all very annoying traits to have outside this scope, so we leave them here.
        use crate::proto::{instruction, operand::Value, Program};

        struct OperandHandler<T>(T, usize);

        impl<T: Iterator<Item = Value>> OperandHandler<T> {
            pub fn take_str(&mut self) -> Result<String, YarnProgramError> {
                const EXPECTED: &str = "String";

                match self.0.next() {
                    Some(Value::String(str)) => Ok(str),
                    Some(Value::Bool(_)) => Err(YarnProgramError::UnexpectedOperandKind(
                        UnexpectedOperandKind {
                            idx: self.1,
                            expected: EXPECTED,
                            found: "bool",
                        },
                    )),
                    Some(Value::Float(_)) => Err(YarnProgramError::UnexpectedOperandKind(
                        UnexpectedOperandKind {
                            idx: self.1,
                            expected: EXPECTED,
                            found: "f32",
                        },
                    )),
                    None => Err(YarnProgramError::MissingOperand {
                        idx: self.1,
                        expected: EXPECTED,
                    }),
                }
            }

            fn take_f32(&mut self) -> Result<f32, YarnProgramError> {
                const EXPECTED: &str = "f32";

                match self.0.next() {
                    Some(Value::Float(v)) => Ok(v),
                    Some(Value::String(_)) => Err(YarnProgramError::UnexpectedOperandKind(
                        UnexpectedOperandKind {
                            idx: self.1,
                            expected: EXPECTED,
                            found: "String",
                        },
                    )),
                    Some(Value::Bool(_)) => Err(YarnProgramError::UnexpectedOperandKind(
                        UnexpectedOperandKind {
                            idx: self.1,
                            expected: EXPECTED,
                            found: "bool",
                        },
                    )),
                    None => Err(YarnProgramError::MissingOperand {
                        idx: self.1,
                        expected: EXPECTED,
                    }),
                }
            }

            fn take_bool(&mut self) -> Result<bool, YarnProgramError> {
                const EXPECTED: &str = "bool";

                match self.0.next() {
                    Some(Value::Bool(v)) => Ok(v),
                    Some(Value::String(_)) => Err(YarnProgramError::UnexpectedOperandKind(
                        UnexpectedOperandKind {
                            idx: self.1,
                            expected: EXPECTED,
                            found: "str",
                        },
                    )),
                    Some(Value::Float(_)) => Err(YarnProgramError::UnexpectedOperandKind(
                        UnexpectedOperandKind {
                            idx: self.1,
                            expected: EXPECTED,
                            found: "f32",
                        },
                    )),
                    None => Err(YarnProgramError::MissingOperand {
                        idx: self.1,
                        expected: EXPECTED,
                    }),
                }
            }

            fn take_floaty_usize(&mut self) -> Result<usize, YarnProgramError> {
                self.take_f32().map(|v| v as usize)
            }
        }

        // we use this horrible syntax because importing `prost::Message` makes autocomplete much worse in this file.
        let program = match <Program as prost::Message>::decode(program) {
            Ok(v) => v,
            Err(e) => {
                return Err(YarnProgramError::DecodeErr(e));
            }
        };

        let mut output_nodes = HashMap::new();

        for (node_name, node) in program.nodes {
            // first up, let's convert the instructions
            let mut instructions = vec![];
            for (i, raw_instruction) in node.instructions.into_iter().enumerate() {
                let Some(opcode) = instruction::OpCode::from_i32(raw_instruction.opcode) else {
                    return Err(YarnProgramError::UnexpectedOpCode { found: raw_instruction.opcode, idx: i });
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
                        f_string: operands.take_str()?,
                        parameter_count: operands.take_floaty_usize()?,
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
                        return Err(YarnProgramError::UnsupportedOpCode(raw_instruction.opcode));
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
                        Value::String(v) => crate::YarnValue::Str(v),
                        Value::Bool(v) => crate::YarnValue::Bool(v),
                        Value::Float(v) => crate::YarnValue::F32(v),
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
}

#[derive(Debug, thiserror::Error)]
pub enum YarnProgramError {
    #[error("unexpected opcode `{found}` at index `{idx}`")]
    // it's an i32 since the C# emits i32s, and if it emits a negative number, it'd be good to know about that
    UnexpectedOpCode { found: i32, idx: usize },

    #[error(transparent)]
    DecodeErr(prost::DecodeError),

    #[error(
        "op-code {0} is no longer supported. please recompile your source with a newer compiler."
    )]
    UnsupportedOpCode(i32),

    #[error(transparent)]
    UnexpectedOperandKind(UnexpectedOperandKind),

    #[error("unexpected operand kind at index `{idx}`: expected `{expected}`")]
    MissingOperand { idx: usize, expected: &'static str },
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
    instructions: Vec<Instruction>,
    /// A jump table, mapping the names of labels to positions in the
    /// instructions list.
    jump_table: HashMap<String, usize>,
    /// The tags associated with this node.
    tags: Vec<String>,
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
    string_key: String,
    substitution_count: usize,
}

#[derive(Debug, Clone, PartialEq)]
pub(crate) struct Command {
    f_string: String,
    parameter_count: usize,
}

#[derive(Debug, Clone, PartialEq)]
pub(crate) struct OptionData {
    line: Line,
    destination: String,
    has_condition: bool,
}

#[cfg(test)]
mod tests {
    use super::*;

    const TEST_PROGRAM: &[u8] = include_bytes!("../proto/test.yarnc");

    #[test]
    fn test() {
        let yarn_runner = YarnProgram::new(TEST_PROGRAM);
        println!("{:#?}", yarn_runner);

        panic!()
    }
}
