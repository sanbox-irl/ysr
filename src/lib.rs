#![doc = include_str!("../README.md")]
// #![cfg_attr(not(feature = "std"), doc = "no-std stand in")]
#![deny(unsafe_code)]
#![deny(rust_2018_idioms)]
// #![deny(missing_docs)]
#![deny(rustdoc::broken_intra_doc_links)]

mod byte_code;
mod proto;

pub use byte_code::{YarnProgram, YarnProgramError};

/// A Virtual Machine which executes a [YarnProgram] via `Iterator`.
#[derive(Debug)]
pub struct YarnRunner {
    /// The current executing yarn program.
    program: YarnProgram,
}

impl YarnRunner {
    /// Creates a new [YarnRunner] with the given Program.
    pub fn new(program: YarnProgram) -> Self {
        Self { program }
    }
}

/// Yarn supports three kinds of values: f32s, bools, and Strings.
#[derive(Debug, PartialEq, PartialOrd, Clone)]
pub enum YarnValue {
    /// A boolean
    Bool(bool),
    /// An owned string
    Str(String),
    /// An f32
    F32(f32),
}
