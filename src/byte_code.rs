use std::collections::HashMap;

use crate::YarnValue;

/// This is the compiled output of the `ysc` compiler, lightly processed by us.
/// Fundamentally, this is the bytecode stream of a yarn program, which the `YarnRunner`
/// can take and actually execute.
#[derive(Debug)]
pub struct YarnProgram {
    name: String,
    nodes: HashMap<String, Node>,
    initial_values: HashMap<String, YarnValue>,
}

impl YarnProgram {
    pub fn new(program: &[u8]) -> Self {
        // these are all very annoying traits to have outside this scope, so we leave them here.
        use crate::proto::{instruction, operand::Value, Program};

        let program = {
            use prost::Message;
            Program::decode(program).unwrap()
        };

        let mut output_nodes = HashMap::new();

        for (node_name, node) in program.nodes {
            // first up, let's convert the instructions
            let mut instructions = vec![];
            for crate::proto::Instruction { opcode, operands } in node.instructions {
                let opcode = instruction::OpCode::from_i32(opcode).unwrap();
                let mut operands = operands.into_iter();

                let instruction = match opcode {
                    instruction::OpCode::JumpTo => {
                        let Some(Value::String(str)) = operands.next().and_then(|v| v.value) else {
                            panic!("unexpected operand kind");
                        };
                        Instruction::JumpTo(str)
                    }
                    instruction::OpCode::Jump => Instruction::Jump,
                    instruction::OpCode::RunLine => {
                        let Some(Value::String(string_key)) = operands.next().and_then(|v| v.value) else {
                            panic!("unexpected operand kind");
                        };

                        let Some(Value::Float(substitution_count)) = operands.next().and_then(|v| v.value) else {
                            panic!("unexpected operand kind");
                        };

                        Instruction::RunLine(Line {
                            string_key,
                            substitution_count: substitution_count as usize,
                        })
                    }
                    instruction::OpCode::RunCommand => {
                        let Some(Value::String(f_string)) = operands.next().and_then(|v| v.value) else {
                            panic!("unexpected operand kind");
                        };

                        let Some(Value::Float(param_count)) = operands.next().and_then(|v| v.value) else {
                            panic!("unexpected operand kind");
                        };

                        Instruction::Command(Command {
                            f_string,
                            parameter_count: param_count as usize,
                        })
                    }
                    instruction::OpCode::AddOption => {
                        let Some(Value::String(string_key)) = operands.next().and_then(|v| v.value) else {
                            panic!("unexpected operand kind");
                        };

                        let Some(Value::String(destination)) = operands.next().and_then(|v| v.value) else {
                            panic!("unexpected operand kind");
                        };

                        let Some(Value::Float(substitution_count)) = operands.next().and_then(|v| v.value) else {
                            panic!("unexpected operand kind");
                        };

                        let Some(Value::Bool(has_condition)) = operands.next().and_then(|v| v.value) else {
                            panic!("unexpected operand kind");
                        };

                        Instruction::AddOption(OptionData {
                            line: Line {
                                string_key,
                                substitution_count: substitution_count as usize,
                            },
                            destination,
                            has_condition,
                        })
                    }
                    instruction::OpCode::ShowOptions => Instruction::ShowOptions,
                    instruction::OpCode::PushString => {
                        let Some(Value::String(str)) = operands.next().and_then(|v| v.value) else {
                            panic!("unexpected operand kind");
                        };

                        Instruction::PushString(str)
                    }
                    instruction::OpCode::PushFloat => {
                        let Some(Value::Float(f)) = operands.next().and_then(|v| v.value) else {
                            panic!("unexpected operand kind");
                        };

                        Instruction::PushFloat(f)
                    }
                    instruction::OpCode::PushBool => {
                        let Some(Value::Bool(b)) = operands.next().and_then(|v| v.value) else {
                            panic!("unexpected operand kind");
                        };

                        Instruction::PushBool(b)
                    }
                    instruction::OpCode::PushNull => {
                        panic!("`push_null` is not supported in ysc 2.2. please recompile.");
                    }
                    instruction::OpCode::JumpIfFalse => {
                        let Some(Value::String(str)) = operands.next().and_then(|v| v.value) else {
                            panic!("unexpected operand kind");
                        };
                        Instruction::JumpIfFalse(str)
                    }
                    instruction::OpCode::Pop => Instruction::Pop,
                    instruction::OpCode::CallFunc => {
                        let Some(Value::String(function_name)) = operands.next().and_then(|v| v.value) else {
                            panic!("unexpected operand kind");
                        };

                        Instruction::CallFunc(function_name)
                    }
                    instruction::OpCode::PushVariable => {
                        let Some(Value::String(variable_name)) = operands.next().and_then(|v| v.value) else {
                            panic!("unexpected operand kind");
                        };

                        Instruction::PushVar(variable_name)
                    }
                    instruction::OpCode::StoreVariable => {
                        let Some(Value::String(variable_name)) = operands.next().and_then(|v| v.value) else {
                            panic!("unexpected operand kind");
                        };

                        Instruction::StoreVar(variable_name)
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

        Self {
            name: program.name,
            nodes: output_nodes,
            initial_values,
        }
    }
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
