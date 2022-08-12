use super::uhppote;

pub const COMMANDS: [&Command; 2] = [
    &Command {
        name: "get-all-controllers",
        func: get_all_controllers,
    },
    &Command {
        name: "get-controller",
        func: get_controller,
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
        Ok(v) => println!("{:?}", v),
        Err(e) => panic!("ERROR  {:?}", e),
    }
}

fn get_controller() {
    match uhppote::get_controller(405419896) {
        Ok(v) => println!("{:?}", v),
        Err(e) => panic!("ERROR  {:?}", e),
    }
}
