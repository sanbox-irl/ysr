use std::{
    iter::{Enumerate, Peekable},
    mem,
    str::Chars,
};

#[allow(dead_code)]
pub fn parse_line_markup(input: &str) -> Vec<Markup> {
    let mut output = vec![];
    let mut stream = TokenStream(input.chars().enumerate().peekable(), input);

    // our working buffer
    let mut assigned_character = false;
    let mut buf = String::new();
    let mut buf_started = 0;

    while let Some((_i, chr)) = stream.next() {
        match chr {
            '\\' => {
                // this means we're about to escape somethin...
                if matches!(stream.peek(), Some((_, '[')) | Some((_, ']'))) {
                    // eat it up!
                    buf.push(stream.next().unwrap().1);
                }
            }
            // '[' => {
            //     // okay, the beginning of an attribute! Let's do dis.
            //     let name =
            // }
            ':' => {
                if assigned_character {
                    buf.push(':');
                } else {
                    assert_eq!(buf_started, 0, "attribute on character? bizarre");
                    let value = mem::take(&mut buf);

                    // okay we need to eat all the whitespace after it...
                    stream.eat_whitespace();

                    let end = stream.peek_index();

                    // and toss it out there!
                    output.push(Markup {
                        name: "character".to_string(),
                        start: 0,
                        end,
                        attributes: vec![MarkupAttribute {
                            name: "name".to_string(),
                            value: MarkupValue::String(value),
                        }],
                    });

                    assigned_character = true;
                    buf_started = end;
                }
            }
            chr => {
                buf.push(chr);
            }
        }
    }

    output
}

#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub struct Markup {
    name: String,
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

/// A struct wrapper for parsing
struct TokenStream<'a>(Peekable<Enumerate<Chars<'a>>>, &'a str);

impl TokenStream<'_> {
    /// Peeks, and if at end of stream, returns len
    pub fn peek_index(&mut self) -> usize {
        self.0.peek().map(|v| v.0).unwrap_or_else(|| self.1.len())
    }

    /// Peeks the underlying iterator
    pub fn peek(&mut self) -> Option<&(usize, char)> {
        self.0.peek()
    }

    /// Advances the underlying iterator
    pub fn next(&mut self) -> Option<(usize, char)> {
        self.0.next()
    }

    /// Eats all whitespace, throwing it away
    pub fn eat_whitespace(&mut self) {
        while let Some((_, chr)) = self.0.peek() {
            if chr.is_whitespace() {
                self.0.next();
            } else {
                break;
            }
        }
    }

    pub fn get_word(&mut self, buf: &mut ) {

    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn no_markup() {
        assert!(parse_line_markup("this has no markup in it").is_empty());
        assert!(parse_line_markup("    ").is_empty());
        assert!(parse_line_markup("-----").is_empty());
    }

    #[test]
    fn character_markup() {
        let output = parse_line_markup("Narrator: this is a simple line");
        assert_eq!(output.len(), 1);

        let chr = &output[0];
        assert_eq!(chr.name, "character");
        assert_eq!(chr.start, 0);
        assert_eq!(chr.end, 10);
        assert_eq!(chr.attributes.len(), 1);
        assert_eq!(chr.attributes[0].name, "name");
        assert_eq!(
            chr.attributes[0].value,
            MarkupValue::String("Narrator".to_string())
        );
    }

    #[test]
    fn character_ws() {
        let output = parse_line_markup("Narrator:          this is a simple line");
        assert_eq!(output.len(), 1);

        let chr = &output[0];
        assert_eq!(chr.name, "character");
        assert_eq!(chr.start, 0);
        assert_eq!(chr.end, 19);
        assert_eq!(chr.attributes.len(), 1);
        assert_eq!(chr.attributes[0].name, "name");
        assert_eq!(
            chr.attributes[0].value,
            MarkupValue::String("Narrator".to_string())
        );
    }

    #[test]
    fn character_no_ws() {
        let output = parse_line_markup("Narrator:this is a simple line");
        assert_eq!(output.len(), 1);

        let chr = &output[0];
        assert_eq!(chr.name, "character");
        assert_eq!(chr.start, 0);
        assert_eq!(chr.end, 9);
        assert_eq!(chr.attributes.len(), 1);
        assert_eq!(chr.attributes[0].name, "name");
        assert_eq!(
            chr.attributes[0].value,
            MarkupValue::String("Narrator".to_string())
        );
    }

    #[test]
    fn escaped_sequences() {
        assert!(parse_line_markup(r#"Here's some brackets \[ \]"#).is_empty());
    }
}
