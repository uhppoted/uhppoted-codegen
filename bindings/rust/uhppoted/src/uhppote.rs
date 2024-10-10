use std::net::Ipv4Addr;
use async_std::future;

use chrono::NaiveDateTime;
use chrono::NaiveDate;
use chrono::NaiveTime;

use decode::*;
use encode::*;
use ut0311::Msg;

use error::Error;

#[path = "encode.rs"]
pub mod encode;

#[path = "decode.rs"]
pub mod decode;

#[path = "ut0311.rs"]
mod ut0311;

#[path = "error.rs"]
pub mod error;

pub type PIN = u32;
pub type Event = decode::{{ CamelCase .model.event.name }};
pub type Result<T> = std::result::Result<T, Error>;
pub type Controller = ut0311::Controller;

#[allow(dead_code)]
trait IController {
    fn resolve(&self) -> ut0311::Controller;
}

{{range .model.responses}}
pub type {{CamelCase .name}} = decode::{{CamelCase .name}};
{{- end}}

pub fn set_bind_addr(addr: &str) {
    ut0311::set_bind_addr(addr)
}

pub fn set_broadcast_addr(addr: &str) {
    ut0311::set_broadcast_addr(addr)
}

pub fn set_listen_addr(addr: &str) {
    ut0311::set_listen_addr(addr)
}

pub fn set_debug(enabled: bool) {
    ut0311::set_debug(enabled)
}

pub async fn get_all_controllers() -> Result<Vec<GetControllerResponse>> {
    let request = get_controller_request(0)?;
    let response = ut0311::broadcast(&request).await;

    match response {
        Ok(replies) => {
            return Ok(replies
                .iter()
                .map(|reply| get_controller_response(&reply))
                .filter_map(|reply| reply.ok())
                .collect::<Vec<_>>());
        },
        Err(e) => return Err(e),
    }
}

pub fn listen(events: fn(Event), errors: fn(error::Error), interrupt: impl future::Future) -> Result<()> {
    let pipe = |msg: Msg| match decode::event(&msg) {
        Ok(event) => events(event),
        Err(e) => errors(e),
    };

    return ut0311::listen(pipe, errors, interrupt);
}

{{range .model.functions}}
{{- template "function" . -}}
{{end -}}

{{define "function"}}
pub async fn {{snakeCase .name}}({{template "args" .args}}) -> {{template "result" .}}{ 
    let request = {{snakeCase .request.name}}(controller.controller, {{template "params" slice .args 1}})?;
    let response = ut0311::send(controller, &request).await;

    {{if .response -}}
    match response {
        Ok(reply) =>  return Ok({{snakeCase .response.name}}(&reply)?),
        Err(e) => return Err(e),    
    }
    {{- else -}}
    match response {
        Ok(_) =>  return Ok(true),
        Err(e) => return Err(e),
    }
    {{end}}
}
{{end}}
