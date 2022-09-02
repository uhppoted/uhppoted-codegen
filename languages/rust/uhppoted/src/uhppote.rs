use std::net::Ipv4Addr;
use async_std::future;

use chrono::NaiveDateTime;
use chrono::NaiveDate;
use chrono::NaiveTime;

use decode::*;
use encode::*;
use udp::Msg;

use error::Error;
use error::ErrorKind::NoResponse;

#[path = "encode.rs"]
mod encode;

#[path = "decode.rs"]
mod decode;

#[path = "udp.rs"]
mod udp;

#[path = "error.rs"]
pub mod error;

pub type Event = decode::{{ CamelCase .model.event.name }};
pub type Result<T> = std::result::Result<T, Error>;

pub fn set_bind_addr(addr: &str) {
    udp::set_bind_addr(addr)
}

pub fn set_broadcast_addr(addr: &str) {
    udp::set_broadcast_addr(addr)
}

pub fn set_listen_addr(addr: &str) {
    udp::set_listen_addr(addr)
}

pub fn set_debug(enabled: bool) {
    udp::set_debug(enabled)
}

pub fn get_all_controllers() -> Result<Vec<GetControllerResponse>> {
    let request = get_controller_request(0)?;
    let replies = udp::send(&request, udp::read_all)?;

    let mut list: Vec<decode::GetControllerResponse> = vec![];

    for reply in replies {
        let response = get_controller_response(&reply)?;

        list.push(response);
    }

    return Ok(list);
}

pub fn listen(events: fn(Event), errors: fn(error::Error), interrupt: impl future::Future) -> Result<()> {
    let pipe = |msg: Msg| match decode::event(&msg) {
        Ok(event) => events(event),
        Err(e) => errors(e),
    };

    return udp::listen(pipe, errors, interrupt);
}

{{range .model.functions}}{{template "function" .}}
{{end}}

{{define "function"}}
pub fn {{snakeCase .name}}({{template "args" .args}}) -> {{template "result" .}}{ {{if .response}}
    let request = {{snakeCase .request.name}}({{template "params" .args}})?;
    let replies = udp::send(&request, udp::read)?;

    for reply in replies {
        let response = {{snakeCase .response.name}}(&reply)?;

        return Ok(response);
    }

    return Err(error::Error::from(NoResponse)); {{else}}
    let request = {{snakeCase .request.name}}({{template "params" .args}})?;
    udp::send(&request, udp::read_none)?;

    return Ok(true); {{end}}
}
{{end}}