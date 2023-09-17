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

And then running such a program is simple. See `examples/terminal.rs` for a more complex interactive terminal example.

```rust,no_run
    loop {
        match yarn_runner.execute(&mut storage).unwrap() {
            ExecutionOutput::Line(line) => {
                let markup_output = localization_handler
                    .line_display_text(&line)
                    .unwrap()
                    .unwrap();
                // display the line
            }
            ExecutionOutput::Options(opts) => {
                // display the line

                // pretend we figure out what option the user wants here
                let chosen_option = todo!();
                yarn_runner.select_option(selection).unwrap();
            }
            ExecutionOutput::Command(cmd) => {
                
            }
            ExecutionOutput::Function(function) => {
                // we can do this because we know that we've implemented the default functions
                let output = ysr::process_built_in_function(&function, &yarn_runner)
                    .unwrap()
                    .unwrap();

                yarn_runner.return_function(output).unwrap();
            }
            ExecutionOutput::Finished => {
                break;
            }
        }
    }
```

## MSRV and Safety

This crate has no MSRV yet. If it sees good adoption, an MSRV policy will be decided.

Additionally, this crate is #![deny(unsafe_code)], since no unsafe code was needed. Changing this policy would constitute a minor breaking change.
