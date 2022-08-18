use std::error::Error;
use std::net::Ipv4Addr;

use chrono::NaiveDateTime;

use decode::*;
use encode::*;
use error::ErrorKind::NoResponse;
use udp::send;

#[path = "encode.rs"]
mod encode;

#[path = "decode.rs"]
mod decode;

#[path = "error.rs"]
mod error;

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
    let replies = send(&request, udp::ReplyType::Multiple)?;

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
pub fn {{snakeCase .Name}}({{template "args" .Args}}) -> {{if .Response}}Result<{{CamelCase .Response.Name}}, Box<dyn Error>> {{else}}Result<bool, Box<dyn Error>>{{end}} { {{if .Response}}
    let request = {{snakeCase .Request.Name}}({{template "params" .Args}})?;
    let replies = send(&request, udp::ReplyType::Single)?;

    for reply in replies {
        let response = {{snakeCase .Response.Name}}(&reply)?;

        return Ok(response);
    }

    return Err(Box::new(error::Error::new(NoResponse)));
    {{else}}
    let request = {{snakeCase .Request.Name}}({{template "params" .Args}})?;
    send(&request, udp::ReplyType::Nothing)?;

    return Ok(true);
    {{end}}
}
{{end}}