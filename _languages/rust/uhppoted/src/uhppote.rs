use std::error::Error;
use std::time::Duration;

#[path = "encode.rs"]
mod encode;

#[path = "decode.rs"]
mod decode;

#[path = "errors.rs"]
mod errors;

#[path = "udp.rs"]
mod udp;

pub fn set_bind_addr(addr: &str) {
    udp::set_bind_addr(addr)
}

pub fn set_broadcast_addr(addr: &str) {
    udp::set_broadcast_addr(addr)
}

pub fn set_debug(enabled: bool) {
    udp::set_debug(enabled)
}

pub fn get_all_controllers() -> Result<Vec<decode::GetControllerResponse>, Box<dyn Error>> {
    let request = encode::get_controller_request(0)?;
    let replies = udp::send(&request, Duration::from_millis(2500))?;

    let mut list: Vec<decode::GetControllerResponse> = vec![];

    for reply in replies {
        let response = decode::get_controller_response(&reply)?;

        list.push(response);
    }

    return Ok(list);
}

pub fn get_controller(device_id: u32) -> Result<decode::GetControllerResponse, Box<dyn Error>> {
    let request = encode::get_controller_request(device_id)?;
    let replies = udp::send(&request, Duration::ZERO)?;

    for reply in replies {
        let response = decode::get_controller_response(&reply)?;

        return Ok(response);
    }

    return Err(Box::new(errors::Oops));
}
