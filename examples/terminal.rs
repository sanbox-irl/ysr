use ysr::{ExecutionOutput, Localization, Markup, Program, Runner, Storage};

const PROGRAM_BYTES: &[u8] = include_bytes!("../assets/test.yarnc");
const LOCALIZATION: &str = include_str!("../assets/test-Lines.csv");

fn main() {
    let mut yarn_runner = Runner::new(Program::new(PROGRAM_BYTES).unwrap(), "first_guy").unwrap();
    let localization_handler = Localization::new(LOCALIZATION).unwrap();
    let mut storage = Storage::new();

    let console = dialoguer::console::Term::stderr();

    loop {
        match yarn_runner.execute(&mut storage).unwrap() {
            ExecutionOutput::Line(line) => {
                let markup_output = localization_handler
                    .line_display_text(&line)
                    .unwrap()
                    .unwrap();
                println!("{}", markup_output.clean_text);

                // now wait for the user to hit enter
                std::io::stdin().read_line(&mut String::new()).unwrap();
                console.clear_line().unwrap();
                console.clear_line().unwrap();
            }
            ExecutionOutput::Options(opts) => {
                let mut selection = dialoguer::Select::new();

                for opt in opts {
                    let v = localization_handler
                        .generate_markup_line(opt.line())
                        .unwrap();
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
            ExecutionOutput::Command(cmd) => {
                println!("Executing command `{}`...jk this is a demo", cmd);
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

    println!("program completed");
}
