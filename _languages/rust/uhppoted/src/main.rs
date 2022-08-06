use std::env;

struct Command {
    name: &'static str,
}

const COMMANDS: [&Command; 2] = [
    &Command {
        name: "get-all-controllers",
    },
    &Command {
        name: "get-controller",
    },
];

fn main() {
    println!("uhppoted-codegen: Rust sample application");

    let args: Vec<String> = env::args().collect();

    if args.len() < 2 {
        usage();
        return;
    }

    for c in &args[1..] {
       println!(">>>>>>>> {}",c)
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
