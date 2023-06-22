use std::{collections::HashMap, error::Error, iter::Peekable, ops::Range, str::CharIndices};

const BACKSLASH: char = '\\';
const LEFT_BRACKET: char = '[';
const RIGHT_BRACKET: char = ']';
const FORWARD_SLASH: char = '/';
const COLON: char = ':';
const EQUALS: char = '=';
const DOT: char = '.';
const DOUBLE_QUOTATION: char = '"';
const UNDERSCORE: char = '_';

const CHARACTER: &str = "character";
const TRIM_WHITESPACE: &str = "trimwhitespace";
const NO_MARKUP: &str = "nomarkup";
const NO_MARKUP_END: &str = "[/nomarkup]";

#[derive(Debug)]
pub struct LineMarkup {
    pub clean_text: String,
    pub attributes: Vec<Attribute>,
}

impl LineMarkup {
    /// Markups the string using builtin text replacement markers (`select`, `plural`, and `ordinal`).
    /// If you want more control or to add your own, see [new_raw](Self::new_raw).
    pub fn new(input: &str) -> Result<Self, MarkupParseErr> {
        Self::new_raw(input, built_in_replacement_markers)
    }

    /// This lets you handle all text replacement yourself, without any builtins or only builtins. You can make
    /// new text replacement markers this way!
    pub fn new_raw(
        input: &str,
        mut replacement_marker_handlers: impl FnMut(
            &mut String,
            &Attribute,
        ) -> Result<bool, Box<dyn Error>>,
    ) -> Result<Self, MarkupParseErr> {
        let mut attributes = vec![];
        let mut open: Vec<Attribute> = vec![];
        let mut stream = TokenStream(input.char_indices().peekable(), input);

        // our working buffer
        let mut assigned_character = false;
        let mut clean_text = String::new();

        while let Some((_, chr)) = stream.next() {
            match chr {
                BACKSLASH => {
                    // this means we're about to escape somethin...
                    if matches!(
                        stream.peek(),
                        Some((_, LEFT_BRACKET)) | Some((_, RIGHT_BRACKET))
                    ) {
                        // eat it up!
                        clean_text.push(stream.next().unwrap().1);
                    }
                }
                LEFT_BRACKET => {
                    let is_closing = stream.consume_if(|chr| chr == FORWARD_SLASH);

                    // that's the close all fella
                    if is_closing && stream.consume_if(|chr| chr == RIGHT_BRACKET) {
                        for mut attribute in open.drain(..) {
                            attribute.range.end = clean_text.chars().count();

                            replacement_marker_handlers(&mut clean_text, &attribute)?;
                            attributes.push(attribute);
                        }

                        continue;
                    }

                    // okay, the beginning of an attribute! Let's do dis.
                    let (attribute_name_range, attribute_name) =
                        stream.consume_ident_with_range()?;

                    // eat up any whitespace
                    stream.eat_whitespace();
                    stream.not_finished()?;

                    if is_closing {
                        assert_eq!(stream.next().unwrap().1, RIGHT_BRACKET);

                        match open.iter().rev().position(|att| att.name == attribute_name) {
                            Some(pos) => {
                                // we have to do this because we've reversed
                                let pos = (open.len() - 1) - pos;

                                let mut attribute = open.remove(pos);
                                attribute.range.end = clean_text.chars().count();

                                replacement_marker_handlers(&mut clean_text, &attribute)?;
                                attributes.push(attribute);

                                continue;
                            }
                            None => {
                                return Err(MarkupParseErr::UnexpectedAttributeClose(
                                    attribute_name,
                                ));
                            }
                        }
                    }

                    let mut attribute = Attribute {
                        name: attribute_name,
                        range: clean_text.chars().count()..usize::MAX,
                        properties: HashMap::new(),
                    };

                    // check if it's self closing...
                    let mut self_closing = stream.consume_if(|chr| chr == FORWARD_SLASH);

                    match stream.peek().unwrap() {
                        // shorthand, time to set an attribute
                        (_, EQUALS) => {
                            stream.next();
                            let value = stream.consume_property_value();

                            // eat up the white space
                            stream.eat_whitespace();

                            // check for self closing marker
                            if stream.consume_if(|chr| chr == FORWARD_SLASH) {
                                self_closing = true;
                            }

                            match stream.next() {
                                // happy
                                Some((_, RIGHT_BRACKET)) => {}
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

                            attribute.properties.insert(attribute_name, value);
                        }
                        // okay, that's it buds
                        (_, RIGHT_BRACKET) => {
                            stream.next();
                        }

                        // name for a property -- we don't really care what kind of character it is
                        // because of utf8 localization reasons
                        _ => {
                            // okay grab all the name property keys we can
                            loop {
                                let property_name = stream.consume_ident()?;

                                stream.eat_whitespace();
                                assert_eq!(stream.next().unwrap().1, EQUALS);
                                stream.eat_whitespace();

                                let value = stream.consume_property_value();

                                attribute.properties.insert(property_name, value);

                                stream.eat_whitespace();
                                stream.not_finished()?;

                                // check for self closing marker
                                if stream.consume_if(|chr| chr == FORWARD_SLASH) {
                                    self_closing = true;
                                }

                                if stream.consume_if(|chr| chr == RIGHT_BRACKET) {
                                    break;
                                }
                            }
                        }
                    }

                    if self_closing {
                        attribute.range.end = attribute.range.start;

                        let trim_whitespace =
                            if let Some(tw_value) = attribute.properties.get(TRIM_WHITESPACE) {
                                matches!(tw_value, MarkupValue::Bool(true))
                            } else {
                                // rule: we trim whitespace if the char before is whitespace
                                // or its the start of a line
                                clean_text
                                    .chars()
                                    .last()
                                    .map(|c| c.is_whitespace())
                                    .unwrap_or(true)
                            };

                        let did_replacement =
                            replacement_marker_handlers(&mut clean_text, &attribute)?;
                        attributes.push(attribute);

                        if trim_whitespace && !did_replacement {
                            // eat one whitespace
                            stream.consume_if(|c| c.is_whitespace());
                        }
                    } else {
                        // check if it's a nomarkup attribute
                        if attribute.name == NO_MARKUP {
                            // okay time to CHUNK
                            let mut found_end = false;

                            while let Some((_, chr)) = stream.next() {
                                clean_text.push(chr);

                                // lazily just keep bopping, with a reverse iteration.
                                // this isn't very fast, but it is what it is.
                                // TODO: this won't work with `[  /nomarkup]` and other whitespace
                                // nonsense, but this is fine for now.
                                if clean_text.ends_with(NO_MARKUP_END) {
                                    found_end = true;

                                    // get it out of there!
                                    for _ in 0..NO_MARKUP_END.len() {
                                        clean_text.pop();
                                    }
                                    break;
                                }
                            }

                            if found_end {
                                attribute.range.end = clean_text.chars().count();
                                replacement_marker_handlers(&mut clean_text, &attribute)?;
                                attributes.push(attribute);
                            } else {
                                return Err(MarkupParseErr::AttributeNotClosed(attribute.name));
                            }
                        } else {
                            // check if it's a character, so we don't double attribute
                            if attribute.name == CHARACTER {
                                if assigned_character {
                                    return Err(
                                        MarkupParseErr::ExplicitCharacterAndImplicitCharacter,
                                    );
                                }

                                // okay now we're going to make sure its well formed.
                                let name_value = attribute
                                    .properties
                                    .get("name")
                                    .ok_or(MarkupParseErr::ExplicitCharacterMalformed)?;
                                if !name_value.is_string() {
                                    return Err(MarkupParseErr::ExplicitCharacterMalformed);
                                }

                                // okay we're good to go!
                                assigned_character = true;
                            }

                            open.push(attribute);
                        }
                    }
                }
                COLON => {
                    clean_text.push(COLON);

                    if assigned_character {
                        continue;
                    } else {
                        // this is to kill the `:` we just pushed
                        let value = clean_text[..clean_text.chars().count() - 1].to_owned();

                        let ws_start = stream.peek_index();
                        stream.eat_whitespace();
                        let ws_end = stream.peek_index();
                        clean_text.push_str(stream.get(ws_start..ws_end).unwrap());

                        // and toss it out there!
                        let attribute = Attribute {
                            name: CHARACTER.to_string(),
                            range: 0..clean_text.chars().count(),
                            properties: [("name".to_string(), MarkupValue::String(value))]
                                .into_iter()
                                .collect(),
                        };
                        replacement_marker_handlers(&mut clean_text, &attribute)?;
                        attributes.push(attribute);

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
    pub name: String,
    pub range: Range<usize>,
    pub properties: HashMap<String, MarkupValue>,
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

impl MarkupValue {
    /// Returns `true` if the markup value is [`I32`].
    ///
    /// [`I32`]: MarkupValue::I32
    #[must_use]
    pub fn is_i32(&self) -> bool {
        matches!(self, Self::I32(..))
    }

    /// Returns `true` if the markup value is [`F32`].
    ///
    /// [`F32`]: MarkupValue::F32
    #[must_use]
    pub fn is_f32(&self) -> bool {
        matches!(self, Self::F32(..))
    }

    /// Returns `true` if the markup value is [`String`].
    ///
    /// [`String`]: MarkupValue::String
    #[must_use]
    pub fn is_string(&self) -> bool {
        matches!(self, Self::String(..))
    }

    /// Returns `true` if the markup value is [`Bool`].
    ///
    /// [`Bool`]: MarkupValue::Bool
    #[must_use]
    pub fn is_bool(&self) -> bool {
        matches!(self, Self::Bool(..))
    }
}

impl std::fmt::Display for MarkupValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            MarkupValue::I32(z) => z.fmt(f),
            MarkupValue::F32(z) => z.fmt(f),
            MarkupValue::String(z) => z.fmt(f),
            MarkupValue::Bool(z) => z.fmt(f),
        }
    }
}
#[derive(Debug, thiserror::Error)]
pub enum MarkupParseErr {
    #[error("attribute `{0}` was not closed")]
    AttributeNotClosed(String),

    #[error("attribute `[/{0}]` was not open, so close was unexpected")]
    UnexpectedAttributeClose(String),

    #[error("an explicit [character] tag is present and an implicit `:` character is present")]
    ExplicitCharacterAndImplicitCharacter,

    #[error("an explicit [character] tag is present, but has malformed attributes. either no `name` is present, or it is not a string")]
    ExplicitCharacterMalformed,

    #[error("markup ended unexpectedly")]
    UnexpectedEnd,

    #[error("attribute encountered a replacement issue: {0}")]
    ReplacementMarkerIssue(#[from] Box<dyn Error>),
}

/// Takes a guess at what it could be
fn markup_value_from_str(s: &str) -> MarkupValue {
    match s {
        "true" => MarkupValue::Bool(true),
        "false" => MarkupValue::Bool(false),
        _ => {
            if s.contains(DOT) {
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
struct TokenStream<'a>(Peekable<CharIndices<'a>>, &'a str);

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

    fn not_finished(&mut self) -> Result<(), MarkupParseErr> {
        match self.peek() {
            Some(_) => Ok(()),
            None => Err(MarkupParseErr::UnexpectedEnd),
        }
    }

    fn get(&self, idx: Range<usize>) -> Option<&str> {
        self.1.get(idx)
    }

    fn consume_property_value(&mut self) -> MarkupValue {
        let start = self.peek_index();

        let quote_start = self.consume_if(|chr| chr == DOUBLE_QUOTATION);
        if quote_start {
            let mut output = String::new();

            // eat until the end quote
            let success = loop {
                match self.0.peek() {
                    Some((_, chr)) => {
                        // escape a double quotation mark
                        if *chr == BACKSLASH {
                            self.0.next();

                            if self
                                .0
                                .peek()
                                .map(|v| v.1 == DOUBLE_QUOTATION)
                                .unwrap_or(false)
                            {
                                output.push(DOUBLE_QUOTATION);
                                self.0.next();
                            }

                            continue;
                        }

                        if *chr == DOUBLE_QUOTATION {
                            self.next();

                            break true;
                        } else {
                            output.push(self.0.next().unwrap().1);
                        }
                    }
                    None => {
                        break false;
                    }
                }
            };

            if !success {
                panic!("we didn't get an end quotation");
            }

            MarkupValue::String(output)
        } else {
            // eat the rest of the word under whitespace or a `]`.
            self.eat_while(|chr| {
                !chr.is_whitespace() && chr != RIGHT_BRACKET && chr != FORWARD_SLASH
            });
            let end = self.peek_index();
            markup_value_from_str(self.get(start..end).unwrap())
        }
    }

    fn consume_ident_with_range(&mut self) -> Result<(Range<usize>, String), MarkupParseErr> {
        self.eat_whitespace();
        self.not_finished()?;

        let name_start = self.peek_index();
        self.eat_while(|chr| {
            chr == UNDERSCORE || !(chr.is_whitespace() || chr.is_ascii_punctuation())
        });
        let end = self.peek_index();

        let attribute_name_range = name_start..end;
        let attribute_name = self.get(attribute_name_range.clone()).unwrap().to_owned();

        Ok((attribute_name_range, attribute_name))
    }

    fn consume_ident(&mut self) -> Result<String, MarkupParseErr> {
        self.consume_ident_with_range().map(|v| v.1)
    }
}

/// Handles all built in replacement markers.
pub fn built_in_replacement_markers(
    txt: &mut String,
    attribute: &Attribute,
) -> Result<bool, Box<dyn Error>> {
    match attribute.name.as_str() {
        "select" => select_replacement(txt, attribute)?,
        _ => return Ok(false),
    };

    Ok(true)
}

/// Handles the `select` built-in text replacement marker. Should be called on an attribute with the name
/// `select`, but doesn't explicitly check for that.
pub fn select_replacement(txt: &mut String, attribute: &Attribute) -> Result<(), Box<dyn Error>> {
    #[derive(Debug, thiserror::Error)]
    enum SelectErr {
        #[error("no `value` property was found")]
        NoValueAttribute,
        #[error("property of `{0}` not found")]
        ValueKeyNotFound(String),
    }

    let value = attribute
        .properties
        .get("value")
        .ok_or_else(|| Box::new(SelectErr::NoValueAttribute))?;

    // todo: should we be doing this?
    let key = value.to_string();

    let prop = attribute
        .properties
        .get(&key)
        .ok_or_else(|| Box::new(SelectErr::ValueKeyNotFound(key.to_owned())))?;

    match prop {
        MarkupValue::I32(i) => {
            txt.push_str(&i.to_string());
        }
        MarkupValue::F32(f) => {
            txt.push_str(&f.to_string());
        }
        MarkupValue::Bool(b) => {
            if *b {
                txt.push_str("true");
            } else {
                txt.push_str("false");
            }
        }
        MarkupValue::String(s) => {
            if s.contains('%') && !s.contains("!%") {
                let new_str = s.replace('%', &key);
                txt.push_str(&new_str);
            } else {
                txt.push_str(s);
            }
        }
    }

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn passthrough() {
        assert!(LineMarkup::new("this has no markup in it")
            .unwrap()
            .attributes
            .is_empty());
        assert!(LineMarkup::new("    ").unwrap().attributes.is_empty());
        assert!(LineMarkup::new("-----").unwrap().attributes.is_empty());
    }

    #[test]
    fn character_markup() {
        let output = LineMarkup::new("Narrator: this is a simple line").unwrap();
        assert_eq!(output.clean_text, "Narrator: this is a simple line");

        let attributes = output.attributes;
        assert_eq!(attributes.len(), 1);

        let chr = &attributes[0];
        assert_eq!(chr.name, "character");
        assert_eq!(chr.range, 0..10);
        assert_eq!(chr.properties.len(), 1);
        assert_eq!(
            *chr.properties.get("name").unwrap(),
            MarkupValue::String("Narrator".to_string())
        );
    }

    #[test]
    fn character_ws() {
        let output = LineMarkup::new("Narrator:          this is a simple line").unwrap();
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
        assert_eq!(
            *chr.properties.get("name").unwrap(),
            MarkupValue::String("Narrator".to_string())
        );
    }

    #[test]
    fn character_no_ws() {
        let output = LineMarkup::new("Narrator:this is a simple line").unwrap();
        assert_eq!(output.clean_text, "Narrator:this is a simple line");
        let attributes = output.attributes;
        assert_eq!(attributes.len(), 1);

        let chr = &attributes[0];
        assert_eq!(chr.name, "character");
        assert_eq!(chr.range, 0..9);
        assert_eq!(chr.properties.len(), 1);
        assert_eq!(
            *chr.properties.get("name").unwrap(),
            MarkupValue::String("Narrator".to_string())
        );
    }

    #[test]
    fn escaped_sequences() {
        assert!(LineMarkup::new(r#"Here's some brackets \[ \]"#)
            .unwrap()
            .attributes
            .is_empty());
    }

    #[test]
    fn basic_attributes() {
        let output = LineMarkup::new("Here's a [wave]guy[/wave]").unwrap();
        assert_eq!(output.clean_text, "Here's a guy");

        let attributes = output.attributes;
        assert_eq!(attributes.len(), 1);

        let one = &attributes[0];
        assert_eq!(one.name, "wave");
        assert_eq!(one.range, 9..output.clean_text.len());
        assert_eq!(one.properties.len(), 0);
    }

    #[test]
    fn basic_two() {
        let output = LineMarkup::new(
            "Here's a [wave] guy[/wave] [other_guy] and there goes another guy[/other_guy]",
        )
        .unwrap();
        assert_eq!(
            output.clean_text,
            "Here's a  guy  and there goes another guy"
        );

        let attributes = output.attributes;
        assert_eq!(attributes.len(), 2);

        let one = &attributes[0];
        assert_eq!(one.name, "wave");
        assert_eq!(one.range, 9..13);
        assert_eq!(one.properties.len(), 0);

        let two = &attributes[1];
        assert_eq!(two.name, "other_guy");
        assert_eq!(two.range, 14..41);
        assert_eq!(two.properties.len(), 0);
    }

    #[test]
    fn shorthand_attribute() {
        let output = LineMarkup::new("Here's a [wave=3] guy[/wave]").unwrap();
        assert_eq!(output.clean_text, "Here's a  guy");
        let attributes = output.attributes;

        let one = &attributes[0];
        assert_eq!(one.name, "wave");
        assert_eq!(one.range, 9..output.clean_text.len());
        assert_eq!(one.properties.len(), 1);

        assert_eq!(*one.properties.get("wave").unwrap(), MarkupValue::I32(3));
    }

    #[test]
    fn shorthand_attribute_f32() {
        let output = LineMarkup::new("Here's a [wave=3.1] guy[/wave]").unwrap();
        assert_eq!(output.clean_text, "Here's a  guy");
        let attributes = output.attributes;

        let one = &attributes[0];
        assert_eq!(one.name, "wave");
        assert_eq!(one.range, 9..output.clean_text.len());
        assert_eq!(one.properties.len(), 1);

        assert_eq!(*one.properties.get("wave").unwrap(), MarkupValue::F32(3.1));
    }

    #[test]
    fn shorthand_attribute_single() {
        let output = LineMarkup::new("Here's a [wave=yolo] guy[/wave]").unwrap();
        assert_eq!(output.clean_text, "Here's a  guy");
        let attributes = output.attributes;

        let one = &attributes[0];
        assert_eq!(one.name, "wave");
        assert_eq!(one.range, 9..output.clean_text.len());
        assert_eq!(one.properties.len(), 1);

        assert_eq!(
            *one.properties.get("wave").unwrap(),
            MarkupValue::String("yolo".to_string())
        );
    }

    #[test]
    fn shorthand_attribute_quotes() {
        let output = LineMarkup::new("Here's a [wave=\"yooloo there\"] guy[/wave]").unwrap();
        assert_eq!(output.clean_text, "Here's a  guy");
        let attributes = output.attributes;

        let one = &attributes[0];
        assert_eq!(one.name, "wave");
        assert_eq!(one.range, 9..output.clean_text.len());
        assert_eq!(one.properties.len(), 1);

        assert_eq!(
            *one.properties.get("wave").unwrap(),
            MarkupValue::String("yooloo there".to_string())
        );
    }

    #[test]
    fn close_all() {
        let output =
            LineMarkup::new("Here's a [wave=\"yooloo there\"][okay=3][another_one]guy[/]").unwrap();
        assert_eq!(output.clean_text, "Here's a guy");
        assert_eq!(output.attributes.len(), 3);
        for attribute in output.attributes {
            assert_eq!(attribute.range.end, output.clean_text.len());
        }
    }

    #[test]
    fn self_closing() {
        let output = LineMarkup::new("Here's a [screen_shake/] guy.").unwrap();
        assert_eq!(output.clean_text, "Here's a guy.");
        assert_eq!(output.attributes.len(), 1);

        assert_eq!(output.attributes[0].range.len(), 0);
        assert_eq!(output.attributes[0].range.start, 9);
        assert_eq!(output.attributes[0].range.end, 9);
        assert_eq!(output.attributes[0].name, "screen_shake");
        assert_eq!(output.attributes[0].properties.len(), 0);
    }

    #[test]
    fn self_closing_prop() {
        let output = LineMarkup::new("Here's a [screen_shake value=0.9/] guy.").unwrap();
        assert_eq!(output.clean_text, "Here's a guy.");
        assert_eq!(output.attributes.len(), 1);

        // second screen shake thing
        assert_eq!(output.attributes[0].range.len(), 0);
        assert_eq!(output.attributes[0].range.start, 9);
        assert_eq!(output.attributes[0].range.end, 9);
        assert_eq!(output.attributes[0].name, "screen_shake");
        assert_eq!(output.attributes[0].properties.len(), 1);
        assert_eq!(
            *output.attributes[0].properties.get("value").unwrap(),
            MarkupValue::F32(0.9)
        );
    }

    #[test]
    fn self_closing_prop_quote() {
        let output = LineMarkup::new("Here's a [screen_shake value=\"0.9\"/] guy.").unwrap();
        assert_eq!(output.clean_text, "Here's a guy.");
        assert_eq!(output.attributes.len(), 1);

        // second screen shake thing
        assert_eq!(output.attributes[0].range.len(), 0);
        assert_eq!(output.attributes[0].range.start, 9);
        assert_eq!(output.attributes[0].range.end, 9);
        assert_eq!(output.attributes[0].name, "screen_shake");
        assert_eq!(output.attributes[0].properties.len(), 1);
        assert_eq!(
            *output.attributes[0].properties.get("value").unwrap(),
            MarkupValue::String("0.9".to_string())
        );
    }

    #[test]
    fn self_shorthand_closing_prop_quote() {
        let output = LineMarkup::new("Here's a [screen_shake=\"0.9\"/] guy.").unwrap();
        assert_eq!(output.clean_text, "Here's a guy.");
        assert_eq!(output.attributes.len(), 1);

        // second screen shake thing
        assert_eq!(output.attributes[0].range.len(), 0);
        assert_eq!(output.attributes[0].range.start, 9);
        assert_eq!(output.attributes[0].range.end, 9);
        assert_eq!(output.attributes[0].name, "screen_shake");
        assert_eq!(output.attributes[0].properties.len(), 1);
        assert_eq!(
            *output.attributes[0].properties.get("screen_shake").unwrap(),
            MarkupValue::String("0.9".to_string())
        );
    }

    #[test]
    fn no_markup() {
        let output =
            LineMarkup::new("Here's a [nomarkup][screen_shake=\"0.9\"/] guy.[/nomarkup]").unwrap();
        assert_eq!(output.clean_text, "Here's a [screen_shake=\"0.9\"/] guy.");
        assert_eq!(output.attributes.len(), 1);

        // second screen shake thing
        assert_eq!(output.attributes[0].range.start, 9);
        assert_eq!(output.attributes[0].range.end, output.clean_text.len());
        assert_eq!(output.attributes[0].name, "nomarkup");
    }

    /// ----- The following tests are based on the Yarn Spinner repo.
    #[test]
    fn markup_parsing() {
        let line_markup = LineMarkup::new("A [b]B[/b]").unwrap();
        assert_eq!(line_markup.clean_text, "A B");
        assert_eq!(line_markup.attributes.len(), 1);
        assert_eq!(line_markup.attributes[0].name, "b");
        assert_eq!(line_markup.attributes[0].range, 2..3);
    }

    #[test]
    fn overlapping_attributes() {
        let line_markup = LineMarkup::new("[a][b][c]X[/b][/a]X[/c]").unwrap();
        assert_eq!(line_markup.clean_text, "XX");
        assert_eq!(line_markup.attributes.len(), 3);

        // we don't promise order of the attributes
        assert!(line_markup.attributes.iter().any(|v| v.name == "a"));
        assert!(line_markup.attributes.iter().any(|v| v.name == "b"));
        assert!(line_markup.attributes.iter().any(|v| v.name == "c"));
    }

    #[test]
    fn text_extraction() {
        let line_markup = LineMarkup::new("A [b]B [c]C[/c][/b]").unwrap();
        assert_eq!(line_markup.clean_text, "A B C");
        assert_eq!(line_markup.attributes.len(), 2);

        let b = line_markup
            .attributes
            .iter()
            .find(|v| v.name == "b")
            .unwrap();
        assert_eq!(&line_markup.clean_text[b.range.clone()], "B C");

        let c = line_markup
            .attributes
            .iter()
            .find(|v| v.name == "c")
            .unwrap();
        assert_eq!(&line_markup.clean_text[c.range.clone()], "C");
    }

    /*
    snip -- in yarn spinner in C#, attribute removal is tested here.
    we don't support attribute remove ourselves -- instead, users can do that.
     */

    #[test]
    fn finding_attributes() {
        let line_markup = LineMarkup::new("A [b]B[/b] [b]C[/b]").unwrap();
        assert_eq!(line_markup.clean_text, "A B C");
        let attribute = line_markup
            .attributes
            .iter()
            .find(|v| v.name == "b")
            .unwrap();

        assert_eq!(*attribute, line_markup.attributes[0]);
        assert_ne!(*attribute, line_markup.attributes[1]);

        assert!(!line_markup.attributes.iter().any(|v| v.name == "c"));
    }

    #[test]
    fn multibyte_character_parsing() {
        fn quick_check(input: &str, clean_txt: &str, attribute_name: &str, rng: Range<usize>) {
            let line_markup = LineMarkup::new(input).unwrap();
            assert_eq!(line_markup.clean_text, clean_txt);
            assert_eq!(line_markup.attributes.len(), 1);
            assert_eq!(line_markup.attributes[0].range, rng);
            assert_eq!(line_markup.attributes[0].name, attribute_name);
        }

        // there are two kinds of accent marks and i want to track both of them
        const STANDARD: char = 'á';
        const WEIRD: &str = "á";

        // boring dummy
        quick_check("S [a]S[/a]", "S S", "a", 2..3);

        quick_check(
            &format!("{WEIRD} [{WEIRD}]S[/{WEIRD}]"),
            &format!("{WEIRD} S"),
            WEIRD,
            3..4,
        );
        quick_check(
            &format!("{WEIRD} [a]{WEIRD}[/a]"),
            &format!("{WEIRD} {WEIRD}"),
            "a",
            3..5,
        );
        quick_check(
            &format!("{WEIRD} [a]S[/a]"),
            &format!("{WEIRD} S"),
            "a",
            3..4,
        );
        quick_check(&format!("S [{WEIRD}]S[/{WEIRD}]"), "S S", WEIRD, 2..3);
        quick_check(
            &format!("S [a]{WEIRD}[/a]"),
            &format!("S {WEIRD}"),
            "a",
            2..4,
        );

        quick_check(
            &format!("{STANDARD} [{STANDARD}]S[/{STANDARD}]"),
            &format!("{STANDARD} S"),
            &STANDARD.to_string(),
            2..3,
        );
        quick_check(
            &format!("{STANDARD} [a]{STANDARD}[/a]"),
            &format!("{STANDARD} {STANDARD}"),
            "a",
            2..3,
        );
        quick_check(
            &format!("{STANDARD} [a]S[/a]"),
            &format!("{STANDARD} S"),
            "a",
            2..3,
        );
        quick_check(
            &format!("S [{STANDARD}]S[/{STANDARD}]"),
            "S S",
            &STANDARD.to_string(),
            2..3,
        );
        quick_check(
            &format!("S [a]{STANDARD}[/a]"),
            &format!("S {STANDARD}"),
            "a",
            2..3,
        );

        // couple of different characters, truly just googling "chinese characters".
        quick_check("漢 [字]?[/字]", "漢 ?", "字", 2..3);
    }

    #[test]
    fn unexpected_close_marker_throws() {
        assert!(matches!(
            LineMarkup::new("[a][/a][/b]").unwrap_err(),
            MarkupParseErr::UnexpectedAttributeClose(v) if v == "b"
        ));

        assert!(matches!(
            LineMarkup::new("[/b]").unwrap_err(),
            MarkupParseErr::UnexpectedAttributeClose(v) if v == "b"
        ));

        assert!(matches!(
            LineMarkup::new("[a][/][/b]").unwrap_err(),
            MarkupParseErr::UnexpectedAttributeClose(v) if v == "b"
        ));
    }

    #[test]
    fn markup_shortcut_property_parsing() {
        let line_markup = LineMarkup::new("[a=1]s[/a]").unwrap();
        assert_eq!(line_markup.clean_text, "s");
        assert_eq!(line_markup.attributes.len(), 1);
        assert_eq!(line_markup.attributes[0].range, 0..1);
        assert_eq!(line_markup.attributes[0].name, "a");
        assert_eq!(line_markup.attributes[0].properties.len(), 1);
        assert_eq!(
            *line_markup.attributes[0].properties.get("a").unwrap(),
            MarkupValue::I32(1)
        );
    }

    #[test]
    fn markup_multiple_property() {
        let line = LineMarkup::new("[a p1=1 p2=2]s[/a]").unwrap();
        let a_attrib = line.attributes.iter().find(|v| v.name == "a").unwrap();
        assert_eq!(a_attrib.properties.len(), 2);

        assert_eq!(*a_attrib.properties.get("p1").unwrap(), MarkupValue::I32(1));
        assert_eq!(*a_attrib.properties.get("p2").unwrap(), MarkupValue::I32(2));
    }

    #[test]
    fn markup_property_parsing() {
        fn tester(input: &str, expected_value: MarkupValue) {
            let line = LineMarkup::new(input).unwrap();

            assert_eq!(
                *line.attributes[0].properties.get("p").unwrap(),
                expected_value
            );
        }

        tester(
            "[a p=\"string\"]s[/a]",
            MarkupValue::String("string".to_owned()),
        );
        tester(
            "[a p=\"str\\\"ing\"]s[/a]",
            MarkupValue::String("str\"ing".to_owned()),
        );
        tester(
            "[a p=string]s[/a]",
            MarkupValue::String("string".to_owned()),
        );
        tester("[a p=42]s[/a]", MarkupValue::I32(42));
        tester("[a p=13.37]s[/a]", MarkupValue::F32(13.37));
        tester("[a p=true]s[/a]", MarkupValue::Bool(true));
        tester("[a p=false]s[/a]", MarkupValue::Bool(false));
    }

    #[test]
    fn test_multiple_attributes() {
        fn test(input: &str) {
            let line = LineMarkup::new(input).unwrap();
            assert_eq!(line.clean_text, "A B C D");

            assert_eq!(line.attributes.len(), 2);

            let b = line.attributes.iter().find(|v| v.name == "b").unwrap();
            assert_eq!(b.range, 2..5);
            assert!(b.properties.is_empty());

            let c = line.attributes.iter().find(|v| v.name == "c").unwrap();
            assert_eq!(c.range, 4..5);
            assert!(c.properties.is_empty());
        }

        test("A [b]B [c]C[/c][/b] D");
        test("A [b]B [c]C[/b][/c] D");
        test("A [b]B [c]C[/] D");
    }

    #[test]
    fn self_closing_attributes() {
        let line = LineMarkup::new("A [a/] B").unwrap();
        assert_eq!(line.clean_text, "A B");
        assert_eq!(line.attributes.len(), 1);

        let atty = &line.attributes[0];
        assert_eq!(atty.name, "a");
        assert!(atty.properties.is_empty());
        assert_eq!(atty.range, 2..2);
    }

    #[test]
    fn trailing_whitespace() {
        assert_eq!(LineMarkup::new("A [a/] B").unwrap().clean_text, "A B");
        assert_eq!(
            LineMarkup::new("A [a trimwhitespace=true/] B")
                .unwrap()
                .clean_text,
            "A B"
        );
        assert_eq!(
            LineMarkup::new("A [a trimwhitespace=false/] B")
                .unwrap()
                .clean_text,
            "A  B"
        );
        // assert_eq!(
        //     LineMarkup::new("A [nomarkup/] B").unwrap().clean_text,
        //     "A  B"
        // );
        assert_eq!(
            LineMarkup::new("A [nomarkup trimwhitespace=false/] B")
                .unwrap()
                .clean_text,
            "A  B"
        );
        assert_eq!(
            LineMarkup::new("A [nomarkup trimwhitespace=true/] B")
                .unwrap()
                .clean_text,
            "A B"
        );
    }

    #[test]
    fn implicit_character_parsing() {
        fn test(input: &str) {
            let line = LineMarkup::new(input).unwrap();
            // assert_eq!(line.clean_text, "Mae: Wow!");
            assert_eq!(line.attributes.len(), 1);

            let atty = &line.attributes[0];
            assert_eq!(atty.name, "character");
            assert_eq!(atty.range, 0..5);
            assert_eq!(atty.properties.len(), 1);

            assert_eq!(
                *atty.properties.get("name").unwrap(),
                MarkupValue::String("Mae".to_string())
            );
        }

        test("Mae: Wow!");
        test("[character name=\"Mae\"]Mae: [/character]Wow!");
    }

    #[test]
    fn no_markup_mode_parsing() {
        let line = LineMarkup::new("S [a]S[/a] [nomarkup][a]S;][/a][/nomarkup]").unwrap();

        assert_eq!(line.clean_text, "S S [a]S;][/a]");

        let a = line.attributes.iter().find(|v| v.name == "a").unwrap();
        assert_eq!(a.range, 2..3);

        let no_markup_atty = line
            .attributes
            .iter()
            .find(|v| v.name == "nomarkup")
            .unwrap();
        assert_eq!(no_markup_atty.range, 4..14);
    }

    #[test]
    fn escaping() {
        let line = LineMarkup::new(r#"[a]hello \[b\]hello\[/b\][/a]"#).unwrap();
        assert_eq!(line.clean_text, "hello [b]hello[/b]");
        assert_eq!(line.attributes.len(), 1);

        let atty = &line.attributes[0];
        assert_eq!(atty.name, "a");
        assert_eq!(atty.range, 0..18);
    }

    #[test]
    fn numeric_properties() {
        let line = LineMarkup::new("[select value=1 1=one 2=two 3=three /]").unwrap();

        assert_eq!(line.attributes.len(), 1);
        let atty = &line.attributes[0];

        assert_eq!(atty.name, "select");
        assert_eq!(atty.properties.len(), 4);

        assert_eq!(*atty.properties.get("value").unwrap(), MarkupValue::I32(1));
        assert_eq!(
            *atty.properties.get("1").unwrap(),
            MarkupValue::String("one".to_string())
        );
        assert_eq!(
            *atty.properties.get("2").unwrap(),
            MarkupValue::String("two".to_string())
        );
        assert_eq!(
            *atty.properties.get("3").unwrap(),
            MarkupValue::String("three".to_string())
        );

        // okay now check if we DID select based on some property
        assert_eq!(line.clean_text, "one");
    }

    #[test]
    fn found() {
        LineMarkup::new(
            r#"Do you have any other [select value=jim jim="character" bob ="actors"/] to show me? I know I need $APPLES to be true for this."#,
        ).unwrap();
    }
}
