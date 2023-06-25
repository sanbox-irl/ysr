use std::{collections::HashMap, path::PathBuf};

const FIRST_LINE: &str = "id,text,file,node,lineNumber";

#[derive(Debug)]
pub struct Localization(HashMap<String, LineData>);

impl Localization {
    /// Creates a new localization handler
    #[allow(clippy::result_unit_err)]
    pub fn new(csv_str: &str) -> Result<Self, LocalizationParseErr> {
        let mut local = HashMap::new();

        // go through the csv_str:
        let mut lines = csv_str.lines().enumerate();

        // first line needs to be what we expect:

        let first_line = lines.next();
        let Some((_, FIRST_LINE)) = first_line else {
            return Err(LocalizationParseErr::HeaderLineMissing);
        };

        for (row_idx, next_line) in lines {
            let mut id = None;
            let mut text = None;
            let mut file = None;
            let mut node = None;

            println!("next line = `{}`", next_line);

            let mut in_quotation = false;
            let mut record_count = 0;
            let mut record_starts = 0;
            let mut had_quote = false;

            for (idx, chr) in next_line.char_indices() {
                println!("chr = {}", chr);
                match chr {
                    ',' => {
                        println!("that's a comma! {} ({})", record_count, record_starts);
                        if in_quotation {
                            // don't worry about it, we just vibe
                        } else {
                            // janky as hell!
                            let data = if had_quote {
                                &next_line[record_starts + 1..idx - 1]
                            } else {
                                &next_line[record_starts..idx]
                            };
                            had_quote = false;

                            match record_count {
                                // line id:
                                0 => {
                                    println!("setting id!");
                                    id = Some(data.to_string());
                                }
                                // text:
                                1 => {
                                    text = Some(data.to_string());
                                }
                                // file:
                                2 => {
                                    file = Some(PathBuf::from(data));
                                }
                                // node name:
                                3 => {
                                    node = Some(data.to_string());
                                }
                                _ => return Err(LocalizationParseErr::ExtraRecord(row_idx)),
                            }

                            record_starts = idx + 1;
                            record_count += 1;
                        }
                    }
                    '"' => {
                        println!("flipping quote flag to {}", !in_quotation);
                        in_quotation = !in_quotation;
                        had_quote = true;
                    }
                    _ => {
                        // just vibe it out
                    }
                }
            }

            // and pull the final bit out...
            let line_number = {
                if record_starts == next_line.len() {
                    return Err(LocalizationParseErr::Missing(
                        row_idx,
                        MissingKind::SrcLineNumber,
                    ));
                }

                let data = &next_line[record_starts..];
                data.parse::<usize>()
                    .map_err(|e| LocalizationParseErr::CouldNotParseLineNumber(row_idx, e))?
            };

            let id = id.ok_or(LocalizationParseErr::Missing(row_idx, MissingKind::LineId))?;
            let text = text.ok_or(LocalizationParseErr::Missing(row_idx, MissingKind::Text))?;
            let file = file.ok_or(LocalizationParseErr::Missing(row_idx, MissingKind::File))?;
            let node = node.ok_or(LocalizationParseErr::Missing(row_idx, MissingKind::Node))?;

            local.insert(
                id,
                LineData {
                    text,
                    file,
                    node,
                    line_number,
                },
            );
        }

        Ok(Self(local))
    }

    /// Finds a given line based on the key
    pub fn find_line(&self, line_key: &str) -> Option<&LineData> {
        self.0.get(line_key)
    }

    /// Finds the right line based on the key and then applies string subs as needed.
    pub fn line(&self, line: &crate::Line) -> Option<String> {
        let txt_base = self.0.get(&line.string_key)?;

        Some(crate::apply_arguments_in_substition(
            &txt_base.text,
            &line.substitutions,
        ))
    }
}

#[derive(Debug)]
pub struct LineData {
    pub text: String,
    pub file: PathBuf,
    pub node: String,
    pub line_number: usize,
}

/// An error association with parsing the localization csv, such as `test-Lines.csv`.
#[derive(Debug, thiserror::Error, Clone)]
pub enum LocalizationParseErr {
    #[error("header line, (`id, text...,lineNumber` is not first line of file)")]
    HeaderLineMissing,
    #[error("extra record (column) in row {0}")]
    ExtraRecord(usize),
    #[error("could not parse lineNumber in row {0}: {1}")]
    CouldNotParseLineNumber(usize, std::num::ParseIntError),
    #[error("missing {1} in row {0}")]
    Missing(usize, MissingKind),
}

/// The kind of the thing missing.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum MissingKind {
    /// Column 0, like `line:09d53cf`
    LineId,
    /// Column 1, like `Narrator: This is a simple line of a dialogue`
    Text,
    /// Column 2, like `file/path/to/yarn.yarn`
    File,
    /// Column 3, like `first_node`
    Node,
    /// Column 4, like `26`
    SrcLineNumber,
}

impl std::fmt::Display for MissingKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let w = match self {
            MissingKind::LineId => "line id",
            MissingKind::Text => "text",
            MissingKind::File => "file path",
            MissingKind::Node => "node name",
            MissingKind::SrcLineNumber => "line number",
        };

        f.pad(w)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn basic() {
        const INPUT: &str = include_str!("../test_files/lines.csv");

        let local = Localization::new(INPUT).unwrap();
        assert_eq!(local.0.len(), 7);

        let first_line = local.0.get("line:09d53cf").unwrap();
        assert_eq!(
            first_line.text,
            "Narrator: This is a simple line of a dialogue."
        );
        assert_eq!(first_line.file.as_os_str(), "test.yarn");
        assert_eq!(first_line.node, "first_guy");
        assert_eq!(first_line.line_number, 7);

        let quoted_line = local.0.get("line:037f467").unwrap();
        assert_eq!(
            quoted_line.text,
            "Great [wave]options[/wave], but let's [screen_shake/] stop."
        );
        assert_eq!(quoted_line.file.as_os_str(), "test.yarn");
        assert_eq!(quoted_line.node, "opts");
        assert_eq!(quoted_line.line_number, 14);
    }
}
