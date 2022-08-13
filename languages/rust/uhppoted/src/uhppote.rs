use std::error::Error;
use std::time::Duration;

use decode::*;
use encode::*;
use errors::*;
use udp::send;

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

pub fn get_all_controllers() -> Result<Vec<GetControllerResponse>, Box<dyn Error>> {
    let request = get_controller_request(0)?;
    let replies = send(&request, Duration::from_millis(2500))?;

    let mut list: Vec<decode::GetControllerResponse> = vec![];

    for reply in replies {
        let response = get_controller_response(&reply)?;

        list.push(response);
    }

    return Ok(list);
}

{{range .model.Functions}}{{template "function" .}}
{{end}}

{{define "function"}}
pub fn {{snakeCase .Name}}({{template "args" .Args}}) -> Result<{{CamelCase .Response.Name}}, Box<dyn Error>> {
    let request = {{snakeCase .Request.Name}}({{template "params" .Args}})?;
    let replies = send(&request, Duration::ZERO)?;

    for reply in replies {
        let response = {{snakeCase .Response.Name}}(&reply)?;

        return Ok(response);
    }

    return Err(Box::new(NoResponse));
}
{{end}}