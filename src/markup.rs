use std::{
    iter::{Enumerate, Peekable},
    ops::Range,
    str::Chars,
};

#[derive(Debug)]
pub struct LineMarkup {
    pub clean_text: String,
    pub attributes: Vec<Attribute>,
}

impl std::str::FromStr for LineMarkup {
    type Err = MarkupParseErr;

    fn from_str(input: &str) -> Result<Self, Self::Err> {
        let mut attributes = vec![];
        let mut open = vec![];
        let mut stream = TokenStream(input.chars().enumerate().peekable(), input);

        // our working buffer
        let mut assigned_character = false;
        let mut clean_text = String::new();

        while let Some((_, chr)) = stream.next() {
            match chr {
                '\\' => {
                    // this means we're about to escape somethin...
                    if matches!(stream.peek(), Some((_, '[')) | Some((_, ']'))) {
                        // eat it up!
                        clean_text.push(stream.next().unwrap().1);
                    }
                }
                '[' => {
                    // okay, the beginning of an attribute! Let's do dis.
                    let (attribute_name_range, attribute_name) = stream.consume_ident_with_range();

                    // eat up any whitespace
                    stream.eat_whitespace();
                    stream.not_finished().expect("will be err");

                    let mut attribute = Attribute {
                        name: attribute_name,
                        range: clean_text.len()..usize::MAX,
                        properties: vec![],
                    };

                    match stream.peek().unwrap() {
                        // shorthand, time to set an attribute
                        (_, '=') => {
                            stream.next();
                            let value = stream.consume_property_value();

                            // eat up the white space
                            stream.eat_whitespace();

                            match stream.next() {
                                // happy
                                Some((_, ']')) => {}
                                // unexpected
                                Some((_, _)) => {
                                    panic!("unexpected end of attribute")
                                }
                                None => {
                                    panic!("unexpected end of input");
                                }
                            }

                            // get the attribute name again to avoid a clone in the happy path
                            let attribute_name =
                                stream.get(attribute_name_range).unwrap().to_owned();

                            attribute.properties.push(Property {
                                name: attribute_name,
                                value,
                            });
                        }
                        // okay, that's it buds
                        (_, ']') => {
                            stream.next();
                        }

                        // name for a property
                        (_, 'a'..='z' | 'A'..='Z' | '0'..='9' | '_') => {
                            // okay grab all the name property keys we can
                            loop {
                                let property_name = stream.consume_ident();

                                stream.eat_whitespace();
                                assert_eq!(stream.next().unwrap().1, '=');
                                stream.eat_whitespace();

                                let value = stream.consume_property_value();

                                attribute.properties.push(Property {
                                    name: property_name,
                                    value,
                                });

                                stream.eat_whitespace();
                                stream.not_finished().expect("will be an error");

                                if stream.consume_if(|chr| chr == ']') {
                                    break;
                                }
                            }
                        }
                        c => {
                            panic!("probably an invalid name for a property {:?}", c);
                        }
                    }

                    open.push(attribute);
                }
                ':' => {
                    clean_text.push(':');

                    if assigned_character {
                        continue;
                    } else {
                        // this is to kill the `:` we just pushed
                        let value = clean_text[..clean_text.len() - 1].to_owned();

                        let ws_start = stream.peek_index();
                        stream.eat_whitespace();
                        let ws_end = stream.peek_index();
                        clean_text.push_str(stream.get(ws_start..ws_end).unwrap());

                        // and toss it out there!
                        attributes.push(Attribute {
                            name: "character".to_string(),
                            range: 0..clean_text.len(),
                            properties: vec![Property {
                                name: "name".to_string(),
                                value: MarkupValue::String(value),
                            }],
                        });

                        assigned_character = true;
                    }
                }
                chr => {
                    clean_text.push(chr);
                }
            }
        }

        // we report the first as an error -- it's not perfect, but good enough.
        if let Some(open) = open.first() {
            return Err(MarkupParseErr::AttributeNotClosed(open.name.clone()));
        }

        Ok(Self {
            clean_text,
            attributes,
        })
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Attribute {
    name: String,
    range: Range<usize>,
    properties: Vec<Property>,
}

#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub struct Property {
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

#[derive(Debug, thiserror::Error, Clone)]
pub enum MarkupParseErr {
    #[error("attribute `{0}` was not closed")]
    AttributeNotClosed(String),
}

/// Takes a guess at what it could be
fn markup_from_str(s: &str) -> MarkupValue {
    match s {
        "true" => MarkupValue::Bool(true),
        "false" => MarkupValue::Bool(false),
        _ => {
            if s.contains('.') {
                if let Ok(f) = s.parse::<f32>() {
                    return MarkupValue::F32(f);
                }
            } else if let Ok(f) = s.parse::<i32>() {
                return MarkupValue::I32(f);
            }

            MarkupValue::String(s.to_owned())
        }
    }
}

/// A struct wrapper for parsing
struct TokenStream<'a>(Peekable<Enumerate<Chars<'a>>>, &'a str);

impl TokenStream<'_> {
    /// Peeks, and if at end of stream, returns `len`
    fn peek_index(&mut self) -> usize {
        self.0.peek().map(|v| v.0).unwrap_or_else(|| self.1.len())
    }

    /// Peeks the underlying iterator
    fn peek(&mut self) -> Option<&(usize, char)> {
        self.0.peek()
    }

    /// Advances the underlying iterator
    fn next(&mut self) -> Option<(usize, char)> {
        self.0.next()
    }

    /// Eats all whitespace, throwing it away
    fn eat_whitespace(&mut self) {
        self.eat_while(|chr| chr.is_whitespace());
    }

    /// advances the stream forward as long as the condition is true. When the next char wouldn't satisfy the predicate,
    /// we break.
    ///
    /// This returns `false` only if we ran out of the stream.
    fn eat_while(&mut self, mut predicate: impl FnMut(char) -> bool) -> bool {
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
    fn consume_if(&mut self, predicate: impl Fn(char) -> bool) -> bool {
        let Some((_, chr)) = self.peek() else { return false; };

        if predicate(*chr) {
            self.next();
            true
        } else {
            false
        }
    }

    fn not_finished(&mut self) -> Result<(), ()> {
        match self.peek() {
            Some(_) => Ok(()),
            None => Err(()),
        }
    }

    fn get(&self, idx: Range<usize>) -> Option<&str> {
        self.1.get(idx)
    }

    fn consume_property_value(&mut self) -> MarkupValue {
        let start = self.peek_index();

        let quote_start = self.consume_if(|chr| chr == '"');
        if quote_start {
            // eat until the end quote
            let success = self.eat_while(|chr| chr != '"');
            if !success {
                panic!("we didn't get an end quotation");
            }
            self.next();
        } else {
            // eat the rest of the word under whitespace or a `]`.
            self.eat_while(|chr| !chr.is_whitespace() && chr != ']');
        }

        let end = self.peek_index();

        let short_hand_attribute_txt = if quote_start {
            // sheer off the quotes!
            self.get((start + 1)..(end - 1)).unwrap()
        } else {
            self.get(start..end).unwrap()
        };

        if quote_start {
            MarkupValue::String(short_hand_attribute_txt.to_owned())
        } else {
            markup_from_str(short_hand_attribute_txt)
        }
    }

    fn consume_ident_with_range(&mut self) -> (Range<usize>, String) {
        self.eat_whitespace();
        self.not_finished().expect("convert to err");

        let name_start = self.peek_index();
        self.eat_while(|chr| chr.is_ascii_alphanumeric() || chr == '_');
        let end = self.peek_index();

        let attribute_name_range = name_start..end;
        let attribute_name = self.get(attribute_name_range.clone()).unwrap().to_owned();

        (attribute_name_range, attribute_name)
    }

    fn consume_ident(&mut self) -> String {
        self.consume_ident_with_range().1
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn no_markup() {
        assert!("this has no markup in it"
            .parse::<LineMarkup>()
            .unwrap()
            .attributes
            .is_empty());
        assert!("    ".parse::<LineMarkup>().unwrap().attributes.is_empty());
        assert!("-----".parse::<LineMarkup>().unwrap().attributes.is_empty());
    }

    #[test]
    fn character_markup() {
        let output = "Narrator: this is a simple line"
            .parse::<LineMarkup>()
            .unwrap();
        assert_eq!(output.clean_text, "Narrator: this is a simple line");

        let attributes = output.attributes;
        assert_eq!(attributes.len(), 1);

        let chr = &attributes[0];
        assert_eq!(chr.name, "character");
        assert_eq!(chr.range, 0..10);
        assert_eq!(chr.properties.len(), 1);
        assert_eq!(chr.properties[0].name, "name");
        assert_eq!(
            chr.properties[0].value,
            MarkupValue::String("Narrator".to_string())
        );
    }

    #[test]
    fn character_ws() {
        let output = "Narrator:          this is a simple line"
            .parse::<LineMarkup>()
            .unwrap();
        assert_eq!(
            output.clean_text,
            "Narrator:          this is a simple line"
        );

        let attributes = output.attributes;
        assert_eq!(attributes.len(), 1);

        let chr = &attributes[0];
        assert_eq!(chr.name, "character");
        assert_eq!(chr.range, 0..19);
        assert_eq!(chr.properties.len(), 1);
        assert_eq!(chr.properties[0].name, "name");
        assert_eq!(
            chr.properties[0].value,
            MarkupValue::String("Narrator".to_string())
        );
    }

    #[test]
    fn character_no_ws() {
        let output = "Narrator:this is a simple line"
            .parse::<LineMarkup>()
            .unwrap();
        assert_eq!(output.clean_text, "Narrator:this is a simple line");
        let attributes = output.attributes;
        assert_eq!(attributes.len(), 1);

        let chr = &attributes[0];
        assert_eq!(chr.name, "character");
        assert_eq!(chr.range, 0..9);
        assert_eq!(chr.properties.len(), 1);
        assert_eq!(chr.properties[0].name, "name");
        assert_eq!(
            chr.properties[0].value,
            MarkupValue::String("Narrator".to_string())
        );
    }

    #[test]
    fn escaped_sequences() {
        assert!(
            r#"Here's some brackets \[ \]"#.parse::<LineMarkup>().unwrap().attributes.is_empty()
        );
    }

    #[test]
    fn basic_attributes() {
        let output = "Here's a [wave] guy".parse::<LineMarkup>().unwrap();
        assert_eq!(output.clean_text, "Here's a  guy");

        let attributes = output.attributes;
        assert_eq!(attributes.len(), 1);

        let one = &attributes[0];
        assert_eq!(one.name, "wave");
        assert_eq!(one.range, 9..usize::MAX);
        assert_eq!(one.properties.len(), 0);
    }

    #[test]
    fn basic_two() {
        let output = "Here's a [wave] guy [other_guy] and there goes another guy"
            .parse::<LineMarkup>()
            .unwrap();
        assert_eq!(
            output.clean_text,
            "Here's a  guy  and there goes another guy"
        );

        let attributes = output.attributes;
        assert_eq!(attributes.len(), 2);

        let one = &attributes[0];
        assert_eq!(one.name, "wave");
        assert_eq!(one.range, 9..usize::MAX);
        assert_eq!(one.properties.len(), 0);

        let two = &attributes[1];
        assert_eq!(two.name, "other_guy");
        assert_eq!(two.range, 14..usize::MAX);
        assert_eq!(two.properties.len(), 0);
    }

    #[test]
    fn shorthand_attribute() {
        let output = "Here's a [wave=3] guy".parse::<LineMarkup>().unwrap();
        assert_eq!(output.clean_text, "Here's a  guy");
        let attributes = output.attributes;

        let one = &attributes[0];
        assert_eq!(one.name, "wave");
        assert_eq!(one.range, 9..usize::MAX);
        assert_eq!(one.properties.len(), 1);

        assert_eq!(one.properties[0].name, "wave");
        assert_eq!(one.properties[0].value, MarkupValue::I32(3));
    }

    #[test]
    fn shorthand_attribute_f32() {
        let output = "Here's a [wave=3.1] guy".parse::<LineMarkup>().unwrap();
        assert_eq!(output.clean_text, "Here's a  guy");
        let attributes = output.attributes;

        let one = &attributes[0];
        assert_eq!(one.name, "wave");
        assert_eq!(one.range, 9..usize::MAX);
        assert_eq!(one.properties.len(), 1);

        assert_eq!(one.properties[0].name, "wave");
        assert_eq!(one.properties[0].value, MarkupValue::F32(3.1));
    }

    #[test]
    fn shorthand_attribute_single() {
        let output = "Here's a [wave=yolo] guy".parse::<LineMarkup>().unwrap();
        assert_eq!(output.clean_text, "Here's a  guy");
        let attributes = output.attributes;

        let one = &attributes[0];
        assert_eq!(one.name, "wave");
        assert_eq!(one.range, 9..usize::MAX);
        assert_eq!(one.properties.len(), 1);

        assert_eq!(one.properties[0].name, "wave");
        assert_eq!(
            one.properties[0].value,
            MarkupValue::String("yolo".to_string())
        );
    }

    #[test]
    fn shorthand_attribute_quotes() {
        let output = "Here's a [wave=\"yooloo there\"] guy"
            .parse::<LineMarkup>()
            .unwrap();
        assert_eq!(output.clean_text, "Here's a  guy");
        let attributes = output.attributes;

        let one = &attributes[0];
        assert_eq!(one.name, "wave");
        assert_eq!(one.range, 9..usize::MAX);
        assert_eq!(one.properties.len(), 1);

        assert_eq!(one.properties[0].name, "wave");
        assert_eq!(
            one.properties[0].value,
            MarkupValue::String("yooloo there".to_string())
        );
    }
}
