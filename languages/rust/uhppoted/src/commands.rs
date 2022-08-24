use chrono::offset::Local;
use chrono::NaiveDate;

use super::error;
use super::uhppote;

const CONTROLLER: u32 = 405419896;
const DOOR: u8 = 3;
const MODE: u8 = 2;
const DELAY: u8 = 10;
const CARD: u32 = 8165538;
const CARD_INDEX: u32 = 3;

pub const COMMANDS: [&Command; 17] = [
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
    &Command {
        name: "get-status",
        func: get_status,
    },
    &Command {
        name: "get-listener",
        func: get_listener,
    },
    &Command {
        name: "set-listener",
        func: set_listener,
    },
    &Command {
        name: "get-door-control",
        func: get_door_control,
    },
    &Command {
        name: "set-door-control",
        func: set_door_control,
    },
    &Command {
        name: "open-door",
        func: open_door,
    },
    &Command {
        name: "get-cards",
        func: get_cards,
    },
    &Command {
        name: "get-card",
        func: get_card,
    },
    &Command {
        name: "get-card-by-index",
        func: get_card_by_index,
    },
    &Command {
        name: "put-card",
        func: put_card,
    },
    &Command {
        name: "delete-card",
        func: delete_card,
    },
    &Command {
        name: "delete-all-cards",
        func: delete_all_cards,
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
                println!("{:#?}", response)
            }
        }
        Err(e) => error(e),
    }
}

fn get_controller() {
    match uhppote::get_controller(CONTROLLER) {
        Ok(v) => println!("{:#?}", v),
        Err(e) => error(e),
    }
}

fn set_ip() {
    let address = "192.168.1.100".parse().unwrap();
    let netmask = "255.255.255.0".parse().unwrap();
    let gateway = "192.168.1.1".parse().unwrap();

    match uhppote::set_ip(CONTROLLER, address, netmask, gateway) {
        Ok(v) => println!("{:#?}", v),
        Err(e) => error(e),
    }
}

fn get_time() {
    match uhppote::get_time(CONTROLLER) {
        Ok(v) => println!("{:#?}", v),
        Err(e) => error(e),
    }
}

fn set_time() {
    let now = Local::now().naive_local();

    match uhppote::set_time(CONTROLLER, now) {
        Ok(v) => println!("{:#?}", v),
        Err(e) => error(e),
    }
}

fn get_status() {
    match uhppote::get_status(CONTROLLER) {
        Ok(v) => println!("{:#?}", v),
        Err(e) => error(e),
    }
}

fn get_listener() {
    match uhppote::get_listener(CONTROLLER) {
        Ok(v) => println!("{:#?}", v),
        Err(e) => error(e),
    }
}

fn set_listener() {
    let address = "192.168.1.100".parse().unwrap();
    let port: u16 = 60001;

    match uhppote::set_listener(CONTROLLER, address, port) {
        Ok(v) => println!("{:#?}", v),
        Err(e) => error(e),
    }
}

fn get_door_control() {
    match uhppote::get_door_control(CONTROLLER, DOOR) {
        Ok(v) => println!("{:#?}", v),
        Err(e) => error(e),
    }
}

fn set_door_control() {
    match uhppote::set_door_control(CONTROLLER, DOOR, MODE, DELAY) {
        Ok(v) => println!("{:#?}", v),
        Err(e) => error(e),
    }
}

fn open_door() {
    match uhppote::open_door(CONTROLLER, DOOR) {
        Ok(v) => println!("{:#?}", v),
        Err(e) => error(e),
    }
}

fn get_cards() {
    match uhppote::get_cards(CONTROLLER) {
        Ok(v) => println!("{:#?}", v),
        Err(e) => error(e),
    }
}

fn get_card() {
    match uhppote::get_card(CONTROLLER, CARD) {
        Ok(v) => println!("{:#?}", v),
        Err(e) => error(e),
    }
}

fn get_card_by_index() {
    match uhppote::get_card_by_index(CONTROLLER, CARD_INDEX) {
        Ok(v) => println!("{:#?}", v),
        Err(e) => error(e),
    }
}

fn put_card() {
    let start = NaiveDate::from_ymd(2022, 1, 1);
    let end = NaiveDate::from_ymd(2022, 12, 31);

    match uhppote::put_card(CONTROLLER, CARD, start, end, 0, 1, 29, 0) {
        Ok(v) => println!("{:#?}", v),
        Err(e) => error(e),
    }
}

fn delete_card() {
    match uhppote::delete_card(CONTROLLER, CARD) {
        Ok(v) => println!("{:#?}", v),
        Err(e) => error(e),
    }
}

fn delete_all_cards() {
    match uhppote::delete_all_cards(CONTROLLER) {
        Ok(v) => println!("{:#?}", v),
        Err(e) => error(e),
    }
}

fn error(e: error::Error) {
    println!();
    println!("   *** ERROR: {e}");
    println!();
}
