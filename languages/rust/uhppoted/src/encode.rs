use std::net::Ipv4Addr;

use chrono::NaiveDateTime;

use super::error;

{{range .model.requests}}{{template "request" .}}
{{end}}

{{define "request"}}
pub fn {{snakeCase .name}}({{template "args" .fields}}) -> Result<[u8; 64], error::Error> {
    let mut packet = [0x00; 64];

    packet[0] = 0x17;
    packet[1] = {{byte2hex .msgtype}};

    {{range .fields}}{{if ne .type "magic"}}pack_{{snakeCase .type}}(&mut packet, {{snakeCase .name}}, {{.offset}});
    {{else}}pack_uint32(&mut packet, 0x55aaaa55, {{.offset}});{{end}}{{end}}

    return Ok(packet)
}
{{end}}

fn pack_uint16(packet: &mut [u8; 64], v: u16, offset: usize) {
    let bytes = v.to_le_bytes();

    packet[offset..offset + 2].clone_from_slice(&bytes);
}

fn pack_uint32(packet: &mut [u8; 64], v: u32, offset: usize) {
    let bytes = v.to_le_bytes();

    packet[offset..offset + 4].clone_from_slice(&bytes);
}

fn pack_ipv4(packet: &mut [u8; 64], v: Ipv4Addr, offset: usize) {
    let addr = v.octets();

    packet[offset..offset + 4].clone_from_slice(&addr);
}

fn pack_datetime(packet: &mut [u8; 64], v: NaiveDateTime, offset: usize) {
    let s = v.format("%Y%m%d%H%M%S");
    let bcd = string2bcd(s.to_string());

    packet[offset..offset + 7].clone_from_slice(&bcd);
}

fn string2bcd(s: String) -> [u8; 7] {
    let mut nibbles:[u8; 14] = [0x00; 14];
    let mut bytes:[u8; 7] = [0x00; 7];
    let mut ix = 0;

    if s.len() %2 != 0 {
        ix = 1;
    }

    for ch in s.chars() {
        nibbles[ix] = bcd(ch);
        ix += 1;
    }

    for (i,pair) in nibbles.chunks(2).enumerate() {
        bytes[i] = (pair[0] << 4) | pair[1];
    }

    return bytes;
}

fn bcd(ch: char) -> u8 {
    match ch {
        '0' => 0,
        '1' => 1,
        '2' => 2,
        '3' => 3,
        '4' => 4,
        '5' => 5,
        '6' => 6,
        '7' => 7,
        '8' => 8,
        '9' => 9,
        _ => panic!("invalid BCD digit {ch}"),
    }
}

