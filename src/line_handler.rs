use std::{collections::HashMap, ops::Range, iter::Peekable};

#[derive(Debug)]
pub struct LineHandler(HashMap<String, String>);

impl LineHandler {
    /// Creates a new localization handler
    pub fn new(csv_str: &str) -> Self {
        let mut local = HashMap::new();

        for record in csv::Reader::from_reader(csv_str.as_bytes())
            .records()
            .filter_map(|v| v.ok())
        {
            local.insert(
                record.get(0).unwrap().to_owned(),
                record.get(1).unwrap().to_owned(),
            );
        }

        Self(local)
    }

    /// Finds a given line based on the key
    pub fn find_line(&self, line_key: &str) -> Option<&String> {
        self.0.get(line_key)
    }

    /// Finds the right line based on the key
    pub fn line(&self, line: &crate::YarnLine) -> Option<String> {
        let txt_base = self.0.get(&line.string_key)?;

        Some(crate::apply_arguments_in_substition(
            txt_base,
            &line.substitutions,
        ))
    }
}

pub fn parse_line_markup(input: &str) -> Vec<Markup> {
    let mut chars = input.chars().enumerate().peekable();

    while let Some((i, chr)) = chars.next() {}

    todo!()
}

/// Consumes a string
fn consume_word(iter: &mut Peekable<impl Iterator<Item = (usize, char)>>) -> String {
    
}


#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub struct Markup {
    start: usize,
    end: usize,
    attributes: Vec<MarkupAttribute>,
}

#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub struct MarkupAttribute {
    name: String,
    value: MarkupValue,
}

/// A markup value. Markup can have distinct integers from floats, so uses a different
/// enumeration than [crate::YarnValue].

#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub enum MarkupValue {
    I32(i32),
    F32(f32),
    String(String),
    Bool(bool),
}
