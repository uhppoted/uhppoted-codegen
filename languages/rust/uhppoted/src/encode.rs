use std::net::Ipv4Addr;

use chrono::NaiveDateTime;

use super::error::Error;
use super::Msg;

{{range .model.requests}}{{template "request" .}}
{{end}}

{{define "request"}}
pub fn {{snakeCase .name}}({{template "args" .fields}}) -> Result<Msg, Error> {
    let mut packet = [0x00; 64];

    packet[0] = 0x17;
    packet[1] = {{byte2hex .msgtype}};

    {{range .fields}}
    {{if ne .type "magic"}}pack_{{snakeCase .type}}(&mut packet, {{snakeCase .name}}, {{.offset}})?;
    {{else}}pack_uint32(&mut packet, 0x55aaaa55, {{.offset}})?;{{end}}{{end}}

    return Ok(packet)
}
{{end}}

fn pack_uint8(packet: &mut Msg, v: u8, offset: usize) -> Result<(), Error> {
    packet[offset] = v;
    Ok(())
}

fn pack_uint16(packet: &mut Msg, v: u16, offset: usize) -> Result<(), Error> {
    let bytes = v.to_le_bytes();

    packet[offset..offset + 2].clone_from_slice(&bytes);
    Ok(())
}

fn pack_uint32(packet: &mut Msg, v: u32, offset: usize) -> Result<(), Error> {
    let bytes = v.to_le_bytes();

    packet[offset..offset + 4].clone_from_slice(&bytes);
    Ok(())
}

fn pack_ipv4(packet: &mut Msg, v: Ipv4Addr, offset: usize) -> Result<(), Error> {
    let addr = v.octets();

    packet[offset..offset + 4].clone_from_slice(&addr);
    Ok(())
}

fn pack_datetime(packet: &mut Msg, v: NaiveDateTime, offset: usize) -> Result<(), Error> {
    let s = v.format("%Y%m%d%H%M%S");
    let bcd = string2bcd(s.to_string());

    packet[offset..offset + 7].clone_from_slice(&bcd?);
    Ok(())
}

fn string2bcd(s: String) -> Result<[u8; 7], Error> {
    let mut nibbles:[u8; 14] = [0x00; 14];
    let mut bytes:[u8; 7] = [0x00; 7];
    let mut ix = 0;

    if s.len() %2 != 0 {
        ix = 1;
    }

    for ch in s.chars() {
        nibbles[ix] = bcd(ch)?;
        ix += 1;
    }

    for (i,pair) in nibbles.chunks(2).enumerate() {
        bytes[i] = (pair[0] << 4) | pair[1];
    }

    return Ok(bytes);
}

fn bcd(ch: char) -> Result<u8, Error> {
    match ch {
        '0' => Ok(0),
        '1' => Ok(1),
        '2' => Ok(2),
        '3' => Ok(3),
        '4' => Ok(4),
        '5' => Ok(5),
        '6' => Ok(6),
        '7' => Ok(7),
        '8' => Ok(8),
        '9' => Ok(9),
        _ => Err(Error::from(format!("invalid BCD digit {ch}"))),
    }
}

