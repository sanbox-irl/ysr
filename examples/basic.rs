use yarn_spinner::{ExecutionOutput, Localization, Markup, Program, Runner, Storage};

const PROGRAM_BYTES: &[u8] = include_bytes!("../test_input/test.yarnc");
const LOCAL_BYTES: &str = include_str!("../test_input/test-Lines.csv");

fn main() {
    let mut storage = Storage::new();

    let mut yarn_runner = Runner::new(Program::new(PROGRAM_BYTES).unwrap());
    let localization_handler = Localization::new(LOCAL_BYTES); // will need an unwrap here eventually instead of crashing on bad csv
    yarn_runner.set_node("first_guy").unwrap();
    let console = dialoguer::console::Term::stderr();

    while let Some(output) = yarn_runner.execute(&mut storage).unwrap() {
        match output {
            ExecutionOutput::Line(line) => {
                let text = localization_handler.line(&line).unwrap();
                // and now we pass the markdown through the markdown fella...
                let markup_output = Markup::new(&text).unwrap();
                println!("{}", markup_output.clean_text);

                // now wait for the user to hit enter
                std::io::stdin().read_line(&mut String::new()).unwrap();
                console.clear_line().unwrap();
                console.clear_line().unwrap();
            }
            ExecutionOutput::Options(opts) => {
                let mut selection = dialoguer::Select::new();

                for opt in opts {
                    let v = localization_handler.line(opt.line()).unwrap();
                    let markup_output = Markup::new(&v).unwrap();

                    let v = match opt.condition_passed() {
                        Some(true) | None => markup_output.clean_text,
                        Some(false) => {
                            format!("(DISABLED) {}", markup_output.clean_text)
                        }
                    };
                    selection.item(v);
                }

                let selection = selection.interact_on(&console).unwrap();
                yarn_runner.select_option(selection).unwrap();
            }
            ExecutionOutput::Command(cmd) => todo!(),
            ExecutionOutput::Function(function) => {
                // we can do this because we know that we've implemented the default functions
                let output = yarn_spinner::process_built_in_function(&function)
                    .unwrap()
                    .unwrap();

                yarn_runner.return_function(output).unwrap();
            }
        }
    }

    println!("program completed");
}
