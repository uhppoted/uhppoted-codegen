use std::env;
use std::process::ExitCode;

use commands::COMMANDS;

#[path = "commands.rs"]
mod commands;

#[path = "uhppote.rs"]
mod uhppote;

fn main() -> ExitCode {
    println!("uhppoted-codegen: Rust sample application");

    let options = ["--debug", "--bind", "--broadcast"];
    let args: Vec<String> = env::args().collect();
    let mut args = &args[1..];

    while args.len() > 0 && options.contains(&args[0].as_str()) {
        let arg = args[0].as_str();

        args = &args[1..];

        match arg {
            "--debug" => uhppote::set_debug(true),

            "--bind" => {
                if args.len() > 0 {
                    uhppote::set_bind_addr(args[0].as_str());
                    args = &args[1..];
                }
            }

            "--broadcast" => {
                if args.len() > 0 {
                    uhppote::set_broadcast_addr(args[0].as_str());
                    args = &args[1..];
                }
            }

            _ => (),
        }
    }

    if args.len() == 0 {
        usage();
        return ExitCode::FAILURE;
    }

    if args.len() == 1 && args[0] == "all" {
        for c in COMMANDS {
            c.exec();
            println!();
        }

        return ExitCode::SUCCESS;
    }

    for cmd in args {
        let ok = match COMMANDS.iter().find(|c| c.name == cmd) {
            Some(c) => {
                c.exec();
                println!();
                true
            }

            None => {
                println!();
                println!("   *** ERROR: invalid command {cmd}");
                println!();
                false
            }
        };

        if !ok {
            return ExitCode::FAILURE;
        }
    }

    return ExitCode::SUCCESS;
}

fn usage() {
    println!();
    println!("  Usage: uhppoted [--debug] [--bind <address>] [--broadcast <address>] [commands]");
    println!();
    println!("    Options:");
    println!("    --debug                Displays sent and received UDP packets");
    println!("    --bind <address>       IPv4 address to which to bind. Defaults to 0.0.0.0");
    println!("    --broadcast <address>  IPv4 address to which for UDP broadcast. Defaults to 255.255.255.255:60000");

    println!("    Commands:");
    for c in COMMANDS {
        println!("      {}", c.name);
    }

    println!();
}
