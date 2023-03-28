use std::{
    iter::{Enumerate, Peekable},
    mem,
    ops::Range,
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

    // let mut cleaned_text = String::with_capacity(input.len());

    while let Some((i, chr)) = stream.next() {
        match chr {
            '\\' => {
                // this means we're about to escape somethin...
                if matches!(stream.peek(), Some((_, '[')) | Some((_, ']'))) {
                    // eat it up!
                    buf.push(stream.next().unwrap().1);
                }
            }
            '[' => {
                // okay, the beginning of an attribute! Let's do dis.
                let start = i;
                stream.eat_while(|chr| chr.is_ascii_alphanumeric());
                let end = stream.peek_index();

                let attribute_name = stream.get(start..end).unwrap();

                // eat up any whitespace
                stream.eat_whitespace();

                match stream.peek() {
                    // shorthand, time to set an attribute
                    Some((start, '=')) => {
                        let start = *start;
                        stream.next();

                        let quote_start = stream.consume_if(|chr| chr == '"');
                        if quote_start {
                            // eat until the end quote
                            let success = stream.eat_while(|chr| chr != '"');
                            if !success {
                                panic!("we didn't get an end quotation");
                            }
                            stream.next();
                            stream.eat_whitespace();
                        }

                        stream.eat_while(|chr| chr != ']');
                    }
                    // okay, that's it buds
                    Some((_, ']')) => {}
                    // name for an attribute
                    Some((_, 'a'..='z' | 'A'..='Z')) => {}
                    Some(_) => {
                        panic!("probably an invalid name for a property");
                    }
                    None => {
                        panic!("unexpected end of text");
                    }
                }

                // if stream.peek().map_or(false, |v| v.1 == )
            }
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
    /// Peeks, and if at end of stream, returns `len`
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
        self.eat_while(|chr| chr.is_whitespace());
    }

    /// advances the stream forward as long as the condition is true. When the next char wouldn't satisfy the predicate,
    /// we break.
    ///
    /// This returns `false` only if we ran out of the stream.
    pub fn eat_while(&mut self, predicate: impl Fn(char) -> bool) -> bool {
        loop {
            match self.0.peek() {
                Some((_, chr)) => {
                    if !predicate(*chr) {
                        break true;
                    } else {
                        self.0.next();
                    }
                }
                None => {
                    break false;
                }
            }
        }
    }

    /// If on peek, the next token matches this, consume it. Returns true if we did it.
    pub fn consume_if(&mut self, predicate: impl Fn(char) -> bool) -> bool {
        let Some((_, chr)) = self.peek() else { return false; };

        if predicate(*chr) {
            self.next();
            true
        } else {
            false
        }
    }

    pub fn get(&self, idx: Range<usize>) -> Option<&str> {
        self.1.get(idx)
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
