# ysc

This crate is a Rust handler for the [yarn-spinner](https://yarnspinner.dev) language, as seen in titles like *Night in the Woods* and *A Short Hike*.

## Quick Start

To install, add the following to your `Cargo.toml`:

```toml
ysr = { git = "https://github.com/sanbox-irl/ysr.git" }
```

First, you must compile a program using [ysc](https://github.com/YarnSpinnerTool/YarnSpinner-Console). Then, you can load the resulting binary and text files like this:

```rust,no_run
const PROGRAM_BYTES: &[u8] = include_bytes!("/path/to/test.yarnc");
const LOCALIZATION: &str = include_str!("/path/to/test-Lines.csv");

let yarn_runner = Runner::new(Program::new(PROGRAM_BYTES).unwrap(), "starting_node_in_program")
    .expect("`starting_node_in_program` did not exist!");

let localization_handler = Localization::new(LOCALIZATION).unwrap();
let storage = Storage::new();
```
