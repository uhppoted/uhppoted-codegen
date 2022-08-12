use std::env;

use commands::COMMANDS;

#[path = "commands.rs"]
mod commands;

#[path = "uhppote.rs"]
mod uhppote;

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
    uhppote::set_debug(true);

    for cmd in args {
        match COMMANDS.iter().find(|c| c.name == cmd) {
            Some(c) => c.exec(),
            None => println!("invalid command {}", cmd),
        }

        println!()
    }
}

fn usage() {
    println!();
    println!("  Usage: uhppoted [--debug] [--bind <address>] [--broadcast <address>] [commands]");
    println!();
    println!("    Options:");
    println!("    --debug                Displays sent and received UDP packets");
    println!("    --bind <address>       IPv4 address to which to bind. Defaults to 0.0.0.0:0");
    println!("    --broadcast <address>  IPv4 address to which for UDP broadcast. Defaults to 255.255.255.255:60000");

    println!("    Supported commands:");
    for c in COMMANDS {
        println!("      {}", c.name);
    }

    println!();
}
