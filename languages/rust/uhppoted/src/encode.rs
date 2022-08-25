use std::net::Ipv4Addr;

use chrono::NaiveDateTime;
use chrono::NaiveDate;

use super::error::Error;
use super::Msg;

pub type Result<T> = std::result::Result<T, Error>;

const MAGIC: u32 = 0x55aaaa55;

#[macro_export]
macro_rules! string2bcd {
    ($bcd:expr, $size:expr) => { 
        {
            let mut nibbles: [u8; 2 * $size] = [0; 2 * $size];
            let mut bytes: [u8; $size] = [0; $size];
            let mut ix = 0;

            if $bcd.len() %2 != 0 {
                ix = 1;
            }

            for ch in $bcd.chars() {
                nibbles[ix] = bcd(ch)?;
                ix += 1;
            }

            for (i,pair) in nibbles.chunks(2).enumerate() {
                bytes[i] = (pair[0] << 4) | pair[1];
            }

            bytes
        } 
    };
}

{{range .model.requests}}{{template "request" .}}
{{end}}

{{define "request"}}
pub fn {{snakeCase .name}}({{template "args" .fields}}) -> Result<Msg> {
    let mut packet = [0x00; 64];

    packet[0] = 0x17;
    packet[1] = {{byte2hex .msgtype}};

    {{range .fields}}
    {{if ne .type "magic"}}{{snakeCase .name}}.pack(&mut packet, {{.offset}})?;
    {{else}}MAGIC.pack(&mut packet, {{.offset}})?;{{end}}{{end}}

    return Ok(packet)
}
{{end}}

trait Pack {
    fn pack(self, packet: &mut Msg, offset: usize) -> Result<()>;
}

impl Pack for u8 {
    fn pack(self, packet: &mut Msg, offset: usize) -> Result<()> {
        packet[offset] = self;
        Ok(())
    }
}

impl Pack for u16 {
    fn pack(self, packet: &mut Msg, offset: usize) -> Result<()> {
        let bytes = self.to_le_bytes();

        packet[offset..offset + 2].clone_from_slice(&bytes);
        Ok(())
    }
}

impl Pack for u32 {
    fn pack(self, packet: &mut Msg, offset: usize) -> Result<()> {
        let bytes = self.to_le_bytes();

        packet[offset..offset + 4].clone_from_slice(&bytes);
        Ok(())
    }
}

impl Pack for Ipv4Addr {
    fn pack(self, packet: &mut Msg, offset: usize) -> Result<()> {
        let addr = self.octets();

        packet[offset..offset + 4].clone_from_slice(&addr);
        Ok(())
    }
}

impl Pack for NaiveDate {
    fn pack(self, packet: &mut Msg, offset: usize) -> Result<()> {
        let s = self.format("%Y%m%d");
        let bcd = string2bcd!(s.to_string(),4);

        packet[offset..offset + 4].clone_from_slice(&bcd);
        Ok(())
    }
}

impl Pack for NaiveDateTime {
    fn pack(self, packet: &mut Msg, offset: usize) -> Result<()> {
        let s = self.format("%Y%m%d%H%M%S");
        let bcd = string2bcd!(s.to_string(),7);

        packet[offset..offset + 7].clone_from_slice(&bcd);
        Ok(())
    }
}

fn bcd(ch: char) -> Result<u8> {
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

