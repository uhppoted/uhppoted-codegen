use std::net::Ipv4Addr;

use chrono::NaiveDate;
use chrono::NaiveTime;
use chrono::NaiveDateTime;

use super::error;
use super::Msg;

#[macro_export]
macro_rules! bcd2string {
    ($slice:expr, $size:expr) => { 
        {
            let mut ix: [u8; 2 * $size] = [0; 2 * $size];
            let mut index = 0;

            for b in $slice {
                ix[index] = (b >> 4 & 0x0f).into();
                ix[index+1] = (b >> 0 & 0x0f).into();
                index += 2;
            }

            let mut chars: [char; 2 * $size] = [' '; 2 * $size];
            for i in 0..2 * $size {
                chars[i] = bcd(ix[i]);
            }

            String::from_iter(chars)
        } 
    };
}

{{range .model.responses}}{{template "response" .}}
{{end}}

{{range .model.responses}}{{template "decode" .}}
{{end}}

{{define "response"}}
#[derive(Clone, Debug)]
pub struct {{CamelCase .name}} { {{range .fields}}
    pub {{snakeCase .name}}: {{template "type" .type}},{{end}}
}
{{end}}

{{define "decode"}}
pub fn {{snakeCase .name}}(packet: &Msg) -> Result<{{CamelCase .name}}, error::Error> {
    if packet.len() != 64 {
        return Err(format!("invalid reply packet length ({})", packet.len()))?;
    }
 
    if packet[1] != {{byte2hex .msgtype}} {
        return Err(format!("invalid reply function code ({:02x})", packet[1]))?;
    }
 
    let response = {{CamelCase .name}}{ {{range .fields}}
        {{snakeCase .name}}: unpack_{{snakeCase .type}}(packet, {{.offset}}),{{end}}
    };

    return Ok(response);
}
{{end}}

fn unpack_uint8(packet: &Msg, offset: usize) -> u8 {
    return packet[offset];
}

fn unpack_uint16(packet: &Msg, offset: usize) -> u16 {
    let mut bytes: [u8; 2] = [0; 2];

    bytes.clone_from_slice(&packet[offset..offset + 2]);

    return u16::from_le_bytes(bytes);
}

fn unpack_uint32(packet: &Msg, offset: usize) -> u32 {
    let mut bytes: [u8; 4] = [0; 4];

    bytes.clone_from_slice(&packet[offset..offset + 4]);

    return u32::from_le_bytes(bytes);
}

fn unpack_bool(packet: &Msg, offset: usize) -> bool {
    return packet[offset] != 0x00;
}

fn unpack_ipv4(packet: &Msg, offset: usize) -> Ipv4Addr {
    let u0 = packet[offset];
    let u1 = packet[offset + 1];
    let u2 = packet[offset + 2];
    let u3 = packet[offset + 3];

    return Ipv4Addr::new(u0, u1, u2, u3);
}

fn unpack_mac(packet: &Msg, offset: usize) -> [u8; 6] {
    let mut mac: [u8; 6] = [0; 6];

    mac.clone_from_slice(&packet[offset..offset + 6]);

    return mac;
}

fn unpack_version(packet: &Msg, offset: usize) -> String {
    let major = packet[offset];
    let minor = packet[offset + 1];

    return format!("v{major:x}.{minor:02x}").to_string();
}

fn unpack_date(packet: &Msg, offset: usize) -> NaiveDate {
    let slice: &[u8; 4] = packet[offset..offset + 4].try_into().unwrap();
    let s = bcd2string!(slice, 4);

    match NaiveDate::parse_from_str(&s, "%Y%m%d") {
        Ok(date) => return date,
        Err(_) => return NaiveDate::default(),
    }
}

fn unpack_shortdate(packet: &Msg, offset: usize) -> NaiveDate {
    let slice: &[u8; 3] = packet[offset..offset + 3].try_into().unwrap();
    let s = format!("20{}", bcd2string!(slice, 3));

    match NaiveDate::parse_from_str(&s, "%Y%m%d") {
        Ok(date) => return date,
        Err(_) => return NaiveDate::default(),
    }
}

fn unpack_time(packet: &Msg, offset: usize) -> NaiveTime {
    let slice: &[u8; 3] = packet[offset..offset + 3].try_into().unwrap();
    let s: String = bcd2string!(slice, 3);

    match NaiveTime::parse_from_str(&s, "%H%M%S") {
        Ok(time) => return time,
        Err(_) => return NaiveTime::default(),
    }
}

fn unpack_datetime(packet: &Msg, offset: usize) -> NaiveDateTime {
    let slice: &[u8; 7] = packet[offset..offset + 7].try_into().unwrap();
    let s: String = bcd2string!(slice, 7);

    match NaiveDateTime::parse_from_str(&s, "%Y%m%d%H%M%S") {
        Ok(datetime) => return datetime,
        Err(_) => return NaiveDateTime::default(),
    }
}

fn bcd(b: u8) -> char {
    match b {
        0 => '0',
        1 => '1',
        2 => '2',
        3 => '3',
        4 => '4',
        5 => '5',
        6 => '6',
        7 => '7',
        8 => '8',
        9 => '9',
        _ => panic!("invalid BCD digit {b}"),
    }
}
