use std::net::Ipv4Addr;

use chrono::NaiveDate;
use chrono::NaiveTime;
use chrono::NaiveDateTime;
use itertools::Itertools;
use regex::Regex;

use super::error::Error;
use super::Msg;

type Result<T> = std::result::Result<T, Error>;

#[macro_export]
macro_rules! bcd2string {
    ($slice:expr) => { 
        {
            let s = format!("{:02x}", $slice.iter().format(""));
            let v = if Regex::new(r"^[0-9]*$").unwrap().is_match(&s) {
                        Ok(s)    
                    } else {
                        Err(Error::from(format!("invalid BCD value {s}")))    
                    };

            v.unwrap()
        } 
    };
}
{{range .model.responses}}
{{- template "response" . -}}
{{end}}

{{- with .model.event}}
#[derive(Clone, Debug)]
pub struct {{CamelCase .name}} { {{range .fields}}
    pub {{snakeCase .name}}: {{template "type" .type}},{{end}}
}
{{end}}

{{- range .model.responses}}
{{- template "decode" . -}}
{{end}}

{{- with .model.event -}}
{{- template "decode" . -}}
{{end -}}

{{define "response"}}
#[derive(Clone, Debug)]
pub struct {{CamelCase .name}} { {{range .fields}}
    pub {{snakeCase .name}}: {{template "type" .type}},{{end}}
}
{{end}}

{{define "decode"}}
pub fn {{snakeCase .name}}(packet: &Msg) -> Result<{{CamelCase .name}}> {
    if packet.len() != 64 {
        return Err(format!("invalid reply packet length ({})", packet.len()))?;
    }
 
    // Ref. v6.62 firmware event
    if packet[0] != 0x17 && (packet[0] != 0x19 || packet[1] != 0x20) {
        return Err(format!("invalid reply start of message byte ({:02x})", packet[0]))?;
    }

    if packet[1] != {{byte2hex .msgtype}} {
        return Err(format!("invalid reply function code ({:02x})", packet[1]))?;
    }
 
    let response = {{CamelCase .name}}{ {{range .fields}}
        {{snakeCase .name}}: unpack_{{snakeCase .type}}(packet, {{.offset}})?,{{end}}
    };

    return Ok(response);
}
{{end -}}

fn unpack_uint8(packet: &Msg, offset: usize) -> Result<u8> {
    return Ok(packet[offset]);
}

fn unpack_uint16(packet: &Msg, offset: usize) -> Result<u16> {
    let mut bytes: [u8; 2] = [0; 2];

    bytes.clone_from_slice(&packet[offset..offset + 2]);

    return Ok(u16::from_le_bytes(bytes));
}

fn unpack_uint32(packet: &Msg, offset: usize) -> Result<u32> {
    let mut bytes: [u8; 4] = [0; 4];

    bytes.clone_from_slice(&packet[offset..offset + 4]);

    return Ok(u32::from_le_bytes(bytes));
}

fn unpack_bool(packet: &Msg, offset: usize) -> Result<bool> {
    return Ok(packet[offset] != 0x00);
}

fn unpack_ipv4(packet: &Msg, offset: usize) -> Result<Ipv4Addr> {
    let u0 = packet[offset];
    let u1 = packet[offset + 1];
    let u2 = packet[offset + 2];
    let u3 = packet[offset + 3];

    return Ok(Ipv4Addr::new(u0, u1, u2, u3));
}

fn unpack_mac(packet: &Msg, offset: usize) -> Result<String> {
    let mac = format!("{:02x}:{:02x}:{:02x}:{:02x}:{:02x}:{:02x}",
    packet[offset],
    packet[offset+1],
    packet[offset+2],
    packet[offset+3],
    packet[offset+4],
    packet[offset+5]);

    return Ok(mac);
}

fn unpack_version(packet: &Msg, offset: usize) -> Result<String> {
    let major = packet[offset];
    let minor = packet[offset + 1];

    return Ok(format!("v{major:x}.{minor:02x}").to_string());
}

fn unpack_date(packet: &Msg, offset: usize) -> Result<NaiveDate> {
    let slice: &[u8; 4] = packet[offset..offset + 4].try_into().unwrap();
    let s = bcd2string!(slice);

    match NaiveDate::parse_from_str(&s, "%Y%m%d") {
        Ok(date) => return Ok(date),
        Err(_) => Err(Error::from(format!("invalid date string {}",s))),
    }
}

fn unpack_shortdate(packet: &Msg, offset: usize) -> Result<NaiveDate> {
    let slice: &[u8; 3] = packet[offset..offset + 3].try_into().unwrap();
    let s = format!("20{}", bcd2string!(slice));

    match NaiveDate::parse_from_str(&s, "%Y%m%d") {
        Ok(date) => return Ok(date),
        Err(_) => Err(Error::from(format!("invalid date string {}",s))),
    }
}

fn unpack_optional_date(packet: &Msg, offset: usize) -> Result<Option<NaiveDate>> {
    let slice: &[u8; 4] = packet[offset..offset + 4].try_into().unwrap();
    let s = bcd2string!(slice);

    match NaiveDate::parse_from_str(&s, "%Y%m%d") {
        Ok(date) => return Ok(Some(date)),
        Err(_) => return Ok(None),
    }
}

fn unpack_datetime(packet: &Msg, offset: usize) -> Result<NaiveDateTime> {
    let slice: &[u8; 7] = packet[offset..offset + 7].try_into().unwrap();
    let s: String = bcd2string!(slice);

    match NaiveDateTime::parse_from_str(&s, "%Y%m%d%H%M%S") {
        Ok(datetime) => return Ok(datetime),
        Err(_) => Err(Error::from(format!("invalid date/time string {}",s))),
    }
}

fn unpack_optional_datetime(packet: &Msg, offset: usize) -> Result<Option<NaiveDateTime>> {
    let slice: &[u8; 7] = packet[offset..offset + 7].try_into().unwrap();
    let s: String = bcd2string!(slice);

    match NaiveDateTime::parse_from_str(&s, "%Y%m%d%H%M%S") {
        Ok(datetime) => return Ok(Some(datetime)),
        Err(_) => Ok(None),
    }
}

fn unpack_time(packet: &Msg, offset: usize) -> Result<NaiveTime> {
    let slice: &[u8; 3] = packet[offset..offset + 3].try_into().unwrap();
    let s: String = bcd2string!(slice);

    match NaiveTime::parse_from_str(&s, "%H%M%S") {
        Ok(time) => return Ok(time),
        Err(_) => Err(Error::from(format!("invalid time string {}",s))),
    }
}

fn unpack_hhmm(packet: &Msg, offset: usize) -> Result<NaiveTime> {
    let slice: &[u8; 2] = packet[offset..offset + 2].try_into().unwrap();
    let s: String = bcd2string!(slice);

    match NaiveTime::parse_from_str(&s, "%H%M") {
        Ok(time) => return Ok(time),
        Err(_) => Err(Error::from(format!("invalid HHmm string {}",s))),
    }
}
