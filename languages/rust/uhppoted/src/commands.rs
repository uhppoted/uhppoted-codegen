use super::uhppote;

pub const COMMANDS: [&Command; 3] = [
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
    match uhppote::get_controller(405419896) {
        Ok(v) => println!("{:?}", v),
        Err(e) => panic!("ERROR  {:?}", e),
    }
}

fn set_ip() {
    let bind = "192.168.1.100".parse().unwrap();
    let netmask = "255.255.255.0".parse().unwrap();
    let gateway = "192.168.1.1".parse().unwrap();

    match uhppote::set_ip(405419896, bind, netmask, gateway) {
        Ok(v) => println!("{:?}", v),
        Err(e) => panic!("ERROR  {:?}", e),
    }
}
