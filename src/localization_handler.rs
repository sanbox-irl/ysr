use std::collections::HashMap;

#[derive(Debug)]
pub struct LocalizationHandler(HashMap<String, String>);

impl LocalizationHandler {
    /// Creates a new localization handler
    pub fn new(csv_str: &str) -> Self {
        let mut local = HashMap::new();

        // skip the first line -- it's the header, but we know the header already
        for line in csv_str.lines().skip(1) {
            let mut split = line.split(',');

            let id = split.next().unwrap();
            let text = split.next().unwrap();

            local.insert(id.to_owned(), text.to_owned());
        }

        Self(local)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn basic() {
        let local = LocalizationHandler::new(include_str!("../test_input/test-Lines.csv"));

        println!("{:#?}", local);
        panic!()
    }
}
