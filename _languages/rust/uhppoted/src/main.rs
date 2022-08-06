use std::env;

#[path = "uhppote.rs"]
mod uhppote;

struct Command {
    name: &'static str,
    func: fn() -> Result<i32, String>,
}

const COMMANDS: [&Command; 2] = [
    &Command {
        name: "get-all-controllers",
        func: || -> Result<i32, String> { uhppote::get_all_controllers() },
    },
    &Command {
        name: "get-controller",
        func: || -> Result<i32, String> { uhppote::get_controller() },
    },
];

fn main() {
    println!("uhppoted-codegen: Rust sample application");

    let args: Vec<String> = env::args().collect();
    let args = &args[1..];

    if args.len() == 0 {
        usage();
        return;
    }

    for cmd in args {
        match COMMANDS.iter().find(|c| c.name == cmd) {
            Some(c) => c.exec(),
            None => println!("invalid command {}", cmd),
        }
    }
}

fn usage() {
    println!();
    println!("  Usage: uhppoted [commands]");
    println!();
    println!("    Supported commands:");

    for c in COMMANDS {
        println!("      {}", c.name);
    }

    println!();
}

impl Command {
    pub fn exec(&self) {
        println!("{}", self.name);
        match (self.func)() {
            Ok(v) => println!("{}", v),
            Err(error) => panic!("ERROR  {:?}", error),
        }
    }
}
