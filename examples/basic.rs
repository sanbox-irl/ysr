use yarn_spinner::{ExecutionOutput, LocalizationHandler, YarnProgram, YarnRunner, YarnStorage};

const PROGRAM_BYTES: &[u8] = include_bytes!("../test_input/test.yarnc");
const LOCAL_BYTES: &str = include_str!("../test_input/test-Lines.csv");

fn main() {
    let mut storage = YarnStorage::new();

    let mut yarn_runner = YarnRunner::new(YarnProgram::new(PROGRAM_BYTES).unwrap());
    let localization_handler = LocalizationHandler::new(LOCAL_BYTES); // will need an unwrap here eventually instead of crashing on bad csv
    yarn_runner.set_node("first_guy").unwrap();

    let console = dialoguer::console::Term::stderr();

    while let Some(output) = yarn_runner.execute(&mut storage).unwrap() {
        match output {
            ExecutionOutput::Line(line) => {
                let text = localization_handler.localize(&line).unwrap();
                println!("{}", text);

                // now wait for the user to hit enter
                std::io::stdin().read_line(&mut String::new()).unwrap();
                console.clear_line().unwrap();
                console.clear_line().unwrap();
            }
            ExecutionOutput::Options(opts) => {
                let mut selection = dialoguer::Select::new();

                for opt in opts {
                    let v = localization_handler.localize(opt.line()).unwrap();
                    let v = match opt.condition_passed() {
                        Some(true) | None => v,
                        Some(false) => {
                            format!("{} (DISABLED)", v)
                        }
                    };
                    selection.item(v);
                }

                let selection = selection.interact_on(&console).unwrap();
                yarn_runner.select_option(selection).unwrap();
            }
            ExecutionOutput::Command(cmd) => todo!(),
        }
    }

    println!("program completed");
}
