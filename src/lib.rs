use crate::proto::operand::{self, Value};

mod proto;

#[derive(Debug)]
pub struct YarnRunner {}

impl YarnRunner {
    pub fn new(program: &[u8]) -> Self {
        use proto::instruction;

        let mut program = {
            use prost::Message;
            proto::Program::decode(program).unwrap()
        };
        let mut node = program.nodes.remove("first_guy").unwrap();
        let proto::Instruction { opcode, operands } = node.instructions.remove(0);

        let mut operands = operands.into_iter();

        match instruction::OpCode::from_i32(opcode).unwrap() {
            instruction::OpCode::JumpTo => {
                let Some(Value::String(str)) = operands.next().and_then(|v| v.value) else {
                    panic!("unexpected operand kind");
                };
                Op::JumpTo(str)
            }
            instruction::OpCode::Jump => Op::Jump,
            instruction::OpCode::RunLine => {
                let Some(Value::String(string_key)) = operands.next().and_then(|v| v.value) else {
                    panic!("unexpected operand kind");
                };

                let Some(Value::Float(substitution_count)) = operands.next().and_then(|v| v.value) else {
                    panic!("unexpected operand kind");
                };

                Op::RunLine(Line {
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

                Op::Command(Command {
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

                Op::AddOption(OptionData {
                    line: Line {
                        string_key,
                        substitution_count: substitution_count as usize,
                    },
                    destination,
                    has_condition,
                })
            }
            instruction::OpCode::ShowOptions => Op::ShowOptions,
            instruction::OpCode::PushString => {
                let Some(Value::String(str)) = operands.next().and_then(|v| v.value) else {
                    panic!("unexpected operand kind");
                };

                Op::PushString(str)
            }
            instruction::OpCode::PushFloat => {
                let Some(Value::Float(f)) = operands.next().and_then(|v| v.value) else {
                    panic!("unexpected operand kind");
                };

                Op::PushFloat(f)
            }
            instruction::OpCode::PushBool => {
                let Some(Value::Bool(b)) = operands.next().and_then(|v| v.value) else {
                    panic!("unexpected operand kind");
                };

                Op::PushBool(b)
            }
            instruction::OpCode::PushNull => {
                panic!("`push_null` is not supported in ysc 2.2. please recompile.");
            }
            instruction::OpCode::JumpIfFalse => {
                let Some(Value::String(str)) = operands.next().and_then(|v| v.value) else {
                    panic!("unexpected operand kind");
                };
                Op::JumpIfFalse(str)
            }
            instruction::OpCode::Pop => Op::Pop,
            instruction::OpCode::CallFunc => todo!(),
            instruction::OpCode::PushVariable => todo!(),
            instruction::OpCode::StoreVariable => todo!(),
            instruction::OpCode::Stop => todo!(),
            instruction::OpCode::RunNode => todo!(),
        };

        println!("{:#?}", program);

        Self {}
    }
}

enum Op {
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
}

struct Line {
    string_key: String,
    substitution_count: usize,
}

struct Command {
    f_string: String,
    parameter_count: usize,
}

struct OptionData {
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
        let yarn_runner = YarnRunner::new(TEST_PROGRAM);
        println!("{:#?}", yarn_runner);

        panic!()
    }
}
