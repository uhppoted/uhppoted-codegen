use std::env;
use std::any::Any;

#[path = "uhppote.rs"]
mod uhppote;

struct Command {
    name: &'static str,
    func: fn() -> Result<Box<dyn Any>, Box<dyn std::error::Error>>,
}

const COMMANDS: [&Command; 2] = [
    &Command {
        name: "get-all-controllers",
        func: || -> Result<Box<dyn Any>, Box<dyn std::error::Error>> { uhppote::get_all_controllers() },
    },
    &Command {
        name: "get-controller",
        func: || -> Result<Box<dyn Any>, Box<dyn std::error::Error>> { uhppote::get_controller(405419896) },
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

    uhppote::set_bind_addr("192.168.1.100:0");
    uhppote::set_broadcast_addr("192.168.1.255:60000");

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
            Ok(v) => println!("{:?}", v),
            Err(error) => panic!("ERROR  {:?}", error),
        }
    }
}
