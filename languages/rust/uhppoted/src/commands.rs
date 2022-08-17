use chrono::offset::Local;

use super::uhppote;

const CONTROLLER: u32 = 405419896;

pub const COMMANDS: [&Command; 5] = [
    &Command {
        name: "get-all-controllers",
        func: get_all_controllers,
    },
    &Command {
        name: "get-controller",
        func: get_controller,
    },
    &Command {
        name: "set-ip",
        func: set_ip,
    },
    &Command {
        name: "get-time",
        func: get_time,
    },
    &Command {
        name: "set-time",
        func: set_time,
    },
];

pub struct Command {
    pub name: &'static str,
    pub func: fn(),
}

impl Command {
    pub fn exec(&self) {
        println!("{}", self.name);
        (self.func)();
    }
}

fn get_all_controllers() {
    match uhppote::get_all_controllers() {
        Ok(list) => {
            for response in list {
                println!("{:?}", response)
            }
        }

        Err(e) => panic!("ERROR  {:?}", e),
    }
}

fn get_controller() {
    match uhppote::get_controller(CONTROLLER) {
        Ok(v) => println!("{:?}", v),
        Err(e) => panic!("ERROR  {:?}", e),
    }
}

fn set_ip() {
    let address = "192.168.1.100".parse().unwrap();
    let netmask = "255.255.255.0".parse().unwrap();
    let gateway = "192.168.1.1".parse().unwrap();

    match uhppote::set_ip(CONTROLLER, address, netmask, gateway) {
        Ok(v) => println!("{:?}", v),
        Err(e) => panic!("ERROR  {:?}", e),
    }
}

fn get_time() {
    match uhppote::get_time(CONTROLLER) {
        Ok(v) => println!("{:?}", v),
        Err(e) => panic!("ERROR  {:?}", e),
    }
}

fn set_time() {
    let now = Local::now().naive_local();

    match uhppote::set_time(CONTROLLER, now) {
        Ok(v) => println!("{:?}", v),
        Err(e) => panic!("ERROR  {:?}", e),
    }
}

