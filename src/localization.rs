use std::{collections::HashMap, path::PathBuf};

const FIRST_LINE: &str = "id,text,file,node,lineNumber";

/// This is the Localization tool, created from a file like `test-Lines.csv`
#[derive(Debug, Clone)]
pub struct Localization(HashMap<String, LineData>);

impl Localization {
    /// Creates a new localization handler from a given `csv` input.
    ///
    /// The provided string should be from the `foo-Lines.csv` produced by the yarn-spinner-compiler.
    /// 
    /// ## Sample
    ///
    /// ```csv
    /// id,text,file,node,lineNumber
    /// line:09d53cf,Narrator: This is a simple line of a dialogue.,test.yarn,first_guy,7
    /// line:029bafb,Character: Here are options.,test.yarn,first_guy,8
    /// ```
    ///
    /// Note that the first line must be the header line.
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

            let mut in_quotation = false;
            let mut record_count = 0;
            let mut buf = String::new();

            let mut chars = next_line.chars().peekable();

            while let Some(chr) = chars.next() {
                match chr {
                    ',' => {
                        if in_quotation {
                            buf.push(',');
                        } else {
                            // janky as hell!

                            match record_count {
                                // line id:
                                0 => {
                                    id = Some(buf.clone());
                                }
                                // text:
                                1 => {
                                    text = Some(buf.clone());
                                }
                                // file:
                                2 => {
                                    file = Some(PathBuf::from(&buf));
                                }
                                // node name:
                                3 => {
                                    node = Some(buf.clone());
                                }
                                _ => return Err(LocalizationParseErr::ExtraRecord(row_idx)),
                            }

                            buf.clear();
                            record_count += 1;
                        }
                    }
                    '"' => {
                        // if it's an escaped quote, just bounce
                        if let Some('"') = chars.peek() {
                            // eat it, and we move on
                            chars.next();

                            buf.push('"');
                        } else {
                            in_quotation = !in_quotation;
                        }
                    }
                    chr => {
                        buf.push(chr);
                    }
                }
            }

            // and pull the final bit out...
            let line_number = {
                if buf.is_empty() {
                    return Err(LocalizationParseErr::Missing(
                        row_idx,
                        MissingKind::SrcLineNumber,
                    ));
                }

                buf.parse::<usize>()
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

    /// Finds a given line based on the key. This is used mostly internally.
    pub fn line_data(&self, line_key: &str) -> Option<&LineData> {
        self.0.get(line_key)
    }

    /// Finds the right line based on the key and then applies string subs as needed.
    pub fn generate_markup_line(&self, line: &crate::Line) -> Option<String> {
        let txt_base = self.line_data(&line.string_key)?;

        Some(crate::apply_arguments_in_substition(
            &txt_base.text,
            &line.substitutions,
        ))
    }

    /// This convenience function finds and performs markup parsing in total.
    /// Most users will only need to use this function, though users can independently
    /// do all of this (it only invokes public functions).
    pub fn line_display_text(
        &self,
        line: &crate::Line,
    ) -> Option<Result<crate::Markup, crate::MarkupParseErr>> {
        let generated_line = self.generate_markup_line(line)?;

        Some(crate::Markup::new(&generated_line))
    }
}

#[derive(Debug, Clone)]
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
        const INPUT: &str = include_str!("./../assets/test-Lines.csv");

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

        let double_quoted = local.0.get("line:0307fc5").unwrap();
        assert_eq!(double_quoted.text, "Here's a \"quote\" to show ya.");
        assert_eq!(double_quoted.file.as_os_str(), "test.yarn");
        assert_eq!(double_quoted.node, "opts");
        assert_eq!(double_quoted.line_number, 18);
    }
}
