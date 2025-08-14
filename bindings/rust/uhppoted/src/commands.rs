use chrono::offset::Local;
use chrono::NaiveDate;
use chrono::NaiveTime;
use async_ctrlc::CtrlC;
use lazy_static::lazy_static;

use std::fmt::Debug;
use std::collections::HashMap;

use super::uhppote;
use uhppote::error;
use uhppote::Controller;

const CONTROLLER: u32 = 405419896;
const DOOR: u8 = 3;
const MODE: u8 = 2;
const DELAY: u8 = 10;
const CARD: u32 = 8165538;
const CARD_INDEX: u32 = 3;
const EVENT_INDEX: u32 = 37;
const TIME_PROFILE_ID: u8 = 29;

pub const COMMANDS: [&Command; 35] = [
    &Command {
        name: "get-all-controllers",
        func: get_all_controllers,
    },
    &Command {
        name: "get-controller",
        func: get_controller,
    },
    &Command {
        name: "set-IPv4",
        func: set_ipv4,
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
        name: "get-door",
        func: get_door,
    },
    &Command {
        name: "set-door",
        func: set_door,
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
        name: "get-card-at-index",
        func: get_card_at_index,
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
    &Command {
        name: "get-event",
        func: get_event,
    },
    &Command {
        name: "get-event-index",
        func: get_event_index,
    },
    &Command {
        name: "set-event-index",
        func: set_event_index,
    },
    &Command {
        name: "record-special-events",
        func: record_special_events,
    },
    &Command {
        name: "get-time-profile",
        func: get_time_profile,
    },
    &Command {
        name: "set-time-profile",
        func: set_time_profile,
    },
    &Command {
        name: "delete-all-time-profiles",
        func: delete_all_time_profiles,
    },
    &Command {
        name: "add-task",
        func: add_task,
    },
    &Command {
        name: "refresh-tasklist",
        func: refresh_tasklist,
    },
    &Command {
        name: "clear-tasklist",
        func: clear_tasklist,
    },
    &Command {
        name: "set-pc-control",
        func: set_pc_control,
    },
    &Command {
        name: "set-interlock",
        func: set_interlock,
    },
    &Command {
        name: "activate-keypads",
        func: activate_keypads,
    },
    &Command {
        name: "set-door-passcodes",
        func: set_door_passcodes,
    },
    &Command {
        name: "get-antipassback",
        func: get_antipassback,
    },
    &Command {
        name: "set-antipassback",
        func: set_antipassback,
    },
    &Command {
        name: "restore-default-parameters",
        func: restore_default_parameters,
    },
    &Command {
        name: "listen",
        func: listen,
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

lazy_static! {
    static ref CONTROLLERS: HashMap<u32, Controller> = HashMap::from([
     (405419896, Controller {
         controller: 405419896,
         address: String::from("192.168.1.100:60000"),
         transport: String::from("tcp"),
     }),
 ]);
}

fn get_all_controllers() {
    match futures::executor::block_on(uhppote::get_all_controllers()) {
        Ok(list) => {
            for response in list {
                println!("{:#?}", response)
            }
        }
        Err(e) => error(e),
    }
}

fn get_controller() {
    print(|| -> Result<uhppote::GetControllerResponse, error::Error> {
        let controller = resolve(CONTROLLER);

        futures::executor::block_on(uhppote::get_controller(controller))
    })
}

fn set_ipv4() {
    print(|| -> Result<uhppote::SetIPv4Response, error::Error> {
        let controller = resolve(CONTROLLER);
        let address = "192.168.1.100".parse().unwrap();
        let netmask = "255.255.255.0".parse().unwrap();
        let gateway = "192.168.1.1".parse().unwrap();

        futures::executor::block_on(uhppote::set_ipv4(controller, address, netmask, gateway))
    })
}

fn get_time() {
    print(|| -> Result<uhppote::GetTimeResponse, error::Error> {
        let controller = resolve(CONTROLLER);

        futures::executor::block_on(uhppote::get_time(controller))
    })
}

fn set_time() {
    print(|| -> Result<uhppote::SetTimeResponse, error::Error> {
        let controller = resolve(CONTROLLER);
        let now = Local::now().naive_local();

        futures::executor::block_on(uhppote::set_time(controller, now))
    })
}

fn get_status() {
    print(|| -> Result<uhppote::GetStatusResponse, error::Error> {
        let controller = resolve(CONTROLLER);

        futures::executor::block_on(uhppote::get_status(controller))
    })
}

fn get_listener() {
    print(|| -> Result<uhppote::GetListenerResponse, error::Error> {
        let controller = resolve(CONTROLLER);

        futures::executor::block_on(uhppote::get_listener(controller))
    })
}

fn set_listener() {
    print(|| -> Result<uhppote::SetListenerResponse, error::Error> {
        let controller = resolve(CONTROLLER);
        let address = "192.168.1.100".parse().unwrap();
        let port: u16 = 60001;
        let interval: u8 = 15;

        futures::executor::block_on(uhppote::set_listener(controller, address, port, interval))
    })
}

fn get_door() {
    print(|| -> Result<uhppote::GetDoorResponse, error::Error> {
        let controller = resolve(CONTROLLER);
        let door = DOOR;

        futures::executor::block_on(uhppote::get_door(controller, door))
    })
}

fn set_door() {
    print(|| -> Result<uhppote::SetDoorResponse, error::Error> {
        let controller = resolve(CONTROLLER);
        let door = DOOR;
        let mode = MODE;
        let delay = DELAY;

        futures::executor::block_on(uhppote::set_door(controller, door, mode, delay))
    })
}

fn open_door() {
    print(|| -> Result<uhppote::OpenDoorResponse, error::Error> {
        let controller = resolve(CONTROLLER);
        let door = DOOR;

        futures::executor::block_on(uhppote::open_door(controller, door))
    })
}

fn get_cards() {
    print(|| -> Result<uhppote::GetCardsResponse, error::Error> {
        let controller = resolve(CONTROLLER);

        futures::executor::block_on(uhppote::get_cards(controller))
    })
}

fn get_card() {
    let controller = resolve(CONTROLLER);
    let card = CARD;

    match futures::executor::block_on(uhppote::get_card(controller, card)) {
        Ok(v) => {
            if v.card == 0 {
                error(uhppote::error::Error::from(format!("card {card} not found")))
            } else {
                println!("{:#?}", v)
            }
        },
        Err(e) => error(e),
    }
}

fn get_card_at_index() {
    let controller = resolve(CONTROLLER);
    let index = CARD_INDEX;

    match futures::executor::block_on(uhppote::get_card_at_index(controller, index)) {
        Ok(v) => {
            if v.card == 0 {
                error(uhppote::error::Error::from(format!("card @ index {index} not found")))
            } else if v.card == 0xffffffff {
                error(uhppote::error::Error::from(format!("card @ index {index} deleted")))
            } else {
                println!("{:#?}", v)
            }
        },
        Err(e) => error(e),
    }
}

fn put_card() {
    print(|| -> Result<uhppote::PutCardResponse, error::Error> {
        let controller = resolve(CONTROLLER);
        let card = CARD;
        let start = NaiveDate::from_ymd_opt(2023, 1, 1).unwrap();
        let end = NaiveDate::from_ymd_opt(2023, 12, 31).unwrap();
        let pin = 7531;

        futures::executor::block_on(uhppote::put_card(controller, card, start, end, 0, 1, 29, 0, pin))
    })
}

fn delete_card() {
    print(|| -> Result<uhppote::DeleteCardResponse, error::Error> {
        let controller = resolve(CONTROLLER);
        let card = CARD;

        futures::executor::block_on(uhppote::delete_card(controller, card))
    })
}

fn delete_all_cards() {
    print(|| -> Result<uhppote::DeleteAllCardsResponse, error::Error> {
        let controller = resolve(CONTROLLER);

        futures::executor::block_on(uhppote::delete_all_cards(controller))
    })
}

fn get_event() {
    let controller = resolve(CONTROLLER);
    let index = EVENT_INDEX;

    match futures::executor::block_on(uhppote::get_event(controller, index)) {
        Ok(v) => {
            if v.event_type == 0xff {
                error(uhppote::error::Error::from(format!("event @ index {index} overwritten")))
            } else if v.index == 0 {
                error(uhppote::error::Error::from(format!("event @ index {index} not found")))
            } else {
                println!("{:#?}", v)
            }
        },
        Err(e) => error(e),
    }
}

fn get_event_index() {
    print(|| -> Result<uhppote::GetEventIndexResponse, error::Error> {
        let controller = resolve(CONTROLLER);

        futures::executor::block_on(uhppote::get_event_index(controller))
    })
}

fn set_event_index() {
    print(|| -> Result<uhppote::SetEventIndexResponse, error::Error> {
        let controller = resolve(CONTROLLER);
        let index = EVENT_INDEX;

        futures::executor::block_on(uhppote::set_event_index(controller, index))
    })
}

fn record_special_events() {
    print(|| -> Result<uhppote::RecordSpecialEventsResponse, error::Error> {
        let controller = resolve(CONTROLLER);
        let enabled = true;

        futures::executor::block_on(uhppote::record_special_events(controller, enabled))
    })
}

fn get_time_profile() {
    let controller = resolve(CONTROLLER);
    let profile_id = TIME_PROFILE_ID;

    match futures::executor::block_on(uhppote::get_time_profile(controller, profile_id)) {
        Ok(v) => {
            if v.profile_id == 0 {
                error(uhppote::error::Error::from(format!("time profile {profile_id} not defined")))
            } else {
                println!("{:#?}", v)
            }
        },
        Err(e) => error(e),
    }
}

fn set_time_profile() {
    print(|| -> Result<uhppote::SetTimeProfileResponse, error::Error> {
        let controller = resolve(CONTROLLER);
        let profile_id = TIME_PROFILE_ID;
        let start = NaiveDate::parse_from_str("2022-01-01", "%Y-%m-%d").unwrap();
        let end = NaiveDate::parse_from_str("2022-12-31", "%Y-%m-%d").unwrap();
        let monday = true;
        let tuesday = true;
        let wednesday = false;
        let thursday = true;
        let friday = false;
        let saturday = false;
        let sunday = true;
        let segment_1_start = NaiveTime::parse_from_str("08:30", "%H:%M").unwrap();
        let segment_1_end = NaiveTime::parse_from_str("11:45", "%H:%M").unwrap();
        let segment_2_start = NaiveTime::parse_from_str("12:45", "%H:%M").unwrap();
        let segment_2_end = NaiveTime::parse_from_str("17:00", "%H:%M").unwrap();
        let segment_3_start = NaiveTime::parse_from_str("18:30", "%H:%M").unwrap();
        let segment_3_end = NaiveTime::parse_from_str("20:15", "%H:%M").unwrap();
        let linked_profile_id: u8 = 37;

        futures::executor::block_on(uhppote::set_time_profile(
        controller,
        profile_id,
        start,
        end,
        monday,
        tuesday,
        wednesday,
        thursday,
        friday,
        saturday,
        sunday,
        segment_1_start,
        segment_1_end,
        segment_2_start,
        segment_2_end,
        segment_3_start,
        segment_3_end,
        linked_profile_id))
    })
}

fn delete_all_time_profiles() {
    print(|| -> Result<uhppote::DeleteAllTimeProfilesResponse, error::Error> {
        let controller = resolve(CONTROLLER);

        futures::executor::block_on(uhppote::delete_all_time_profiles(controller))
    })
}

fn add_task() {
    print(|| -> Result<uhppote::AddTaskResponse, error::Error> {
        let controller = resolve(CONTROLLER);
        let start_date = NaiveDate::parse_from_str("2023-01-01", "%Y-%m-%d").unwrap();
        let end_date = NaiveDate::parse_from_str("2023-12-31", "%Y-%m-%d").unwrap();
        let monday = true;
        let tuesday = true;
        let wednesday = false;
        let thursday = true;
        let friday = false;
        let saturday = false;
        let sunday = true;
        let start_time = NaiveTime::parse_from_str("08:30", "%H:%M").unwrap();
        let door = DOOR;
        let task_type: u8 = 2;
        let more_cards: u8 = 0;

        futures::executor::block_on(uhppote::add_task(
        controller, start_date, end_date, monday, tuesday, wednesday, thursday, friday, saturday,
        sunday, start_time, door, task_type, more_cards))
    })
}

fn refresh_tasklist() {
    print(|| -> Result<uhppote::RefreshTasklistResponse, error::Error> {
	    let controller = resolve(CONTROLLER);

        futures::executor::block_on(uhppote::refresh_tasklist(controller))
    })
}

fn clear_tasklist() {
    print(|| -> Result<uhppote::ClearTasklistResponse, error::Error> {
	    let controller = resolve(CONTROLLER);

        futures::executor::block_on(uhppote::clear_tasklist(controller))
    })
}

fn set_pc_control() {
    print(|| -> Result<uhppote::SetPcControlResponse, error::Error> {
	    let controller = resolve(CONTROLLER);
        let enabled = true;

        futures::executor::block_on(uhppote::set_pc_control(controller, enabled))
    })
}

fn set_interlock() {
    print(|| -> Result<uhppote::SetInterlockResponse, error::Error> {
	    let controller = resolve(CONTROLLER);
        let interlock = 3;

        futures::executor::block_on(uhppote::set_interlock(controller, interlock))
    })
}

fn activate_keypads() {
    print(|| -> Result<uhppote::ActivateKeypadsResponse, error::Error> {
	    let controller = resolve(CONTROLLER);
        let reader1 = true;
        let reader2 = true;
        let reader3 = true;
        let reader4 = true;

        futures::executor::block_on(uhppote::activate_keypads(controller, reader1, reader2, reader3, reader4))
    })
}

fn set_door_passcodes() {
    print(|| -> Result<uhppote::SetDoorPasscodesResponse, error::Error> {
	    let controller = resolve(CONTROLLER);
        let door = DOOR;
        let passcode1 = 12345;
        let passcode2 = 0;
        let passcode3 = 999999;
        let passcode4 = 54321;

        futures::executor::block_on(uhppote::set_door_passcodes(controller, door, passcode1, passcode2, passcode3, passcode4))
    })
}

fn get_antipassback() {
    print(|| -> Result<uhppote::GetAntipassbackResponse, error::Error> {
        let controller = resolve(CONTROLLER);

        futures::executor::block_on(uhppote::get_antipassback(controller))
    })
}

fn set_antipassback() {
    print(|| -> Result<uhppote::SetAntipassbackResponse, error::Error> {
        let controller = resolve(CONTROLLER);
        let antipassback = 2;

        futures::executor::block_on(uhppote::set_antipassback(controller, antipassback))
    })
}

fn restore_default_parameters() {
    print(|| -> Result<uhppote::RestoreDefaultParametersResponse, error::Error> {
	    let controller = resolve(CONTROLLER);

        futures::executor::block_on(uhppote::restore_default_parameters(controller))
    })
}

fn listen() {
    let events = |event: uhppote::Event| {
        println!("{:?}", println!("{:#?}", event));
    };

    let errors = |err: uhppote::error::Error| {
        println!("   ERROR: {err}");
    };

    let interrupt = CtrlC::new().expect("cannot create Ctrl+C handler?");
    
    match uhppote::listen(events, errors, interrupt) {
        Ok(v) => println!("{:#?}", v),
        Err(e) => error(e),
    }
}

fn resolve(controller: u32) -> uhppote::Controller {
    if let Some(v) = CONTROLLERS.get(&controller){
        v.clone()
    } else {
        uhppote::Controller {
            controller: controller,
            address: "".to_string(),
            transport: "udp".to_string(),
        }
    }
}

fn print<T: Debug, F>(f: F) where F: Fn() -> Result<T, error::Error> {
    match f() {
        Ok(v) => println!("{:#?}", v),
        Err(e) => error(e),
    }
}

fn error(e: uhppote::error::Error) {
    println!();
    println!("   *** ERROR: {e}");
    println!();
}
