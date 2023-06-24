use std::collections::HashMap;

#[derive(Debug)]
pub struct Localization(HashMap<String, String>);

impl Localization {
    /// Creates a new localization handler
    #[allow(clippy::result_unit_err)]
    pub fn new(csv_str: &str) -> Result<Self, ()> {
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

        Ok(Self(local))
    }

    /// Finds a given line based on the key
    pub fn find_line(&self, line_key: &str) -> Option<&String> {
        self.0.get(line_key)
    }

    /// Finds the right line based on the key and then applies string subs as needed.
    pub fn line(&self, line: &crate::Line) -> Option<String> {
        let txt_base = self.0.get(&line.string_key)?;

        Some(crate::apply_arguments_in_substition(
            txt_base,
            &line.substitutions,
        ))
    }
}
