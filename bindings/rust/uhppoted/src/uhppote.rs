use std::net::Ipv4Addr;
use async_std::future;

use chrono::NaiveDateTime;
use chrono::NaiveDate;
use chrono::NaiveTime;

use decode::*;
use encode::*;
use udp::Msg;

use error::Error;

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

    match futures::executor::block_on(udp::broadcast(&request)) {
        Ok(replies) => {
            let list = replies
                .iter()
                .map(|reply| get_controller_response(&reply))
                .filter_map(|reply| reply.ok())
                .collect::<Vec<_>>();

            return Ok(list);
        }

        Err(e) => return Err(e),
    }
}

pub fn listen(events: fn(Event), errors: fn(error::Error), interrupt: impl future::Future) -> Result<()> {
    let pipe = |msg: Msg| match decode::event(&msg) {
        Ok(event) => events(event),
        Err(e) => errors(e),
    };

    return udp::listen(pipe, errors, interrupt);
}

{{range .model.functions}}
{{- template "function" . -}}
{{end -}}

{{define "function"}}
pub fn {{snakeCase .name}}({{template "args" .args}}) -> {{template "result" .}}{ 
    let request = {{snakeCase .request.name}}({{template "params" .args}})?;

    {{if .response -}}
    match futures::executor::block_on(udp::send(&request)) {
        Ok(reply) =>  return Ok({{snakeCase .response.name}}(&reply)?),
        Err(e) => return Err(e),
    }
    {{- else -}}
    match futures::executor::block_on(udp::send(&request)) {
        Ok(_) =>  return Ok(true),
        Err(e) => return Err(e),
    }
    {{end}}
}
{{end}}