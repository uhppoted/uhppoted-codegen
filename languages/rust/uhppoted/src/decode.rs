use chrono::NaiveDate;
use std::error::Error;
use std::net::Ipv4Addr;

{{range .model.Responses}}{{template "response" .}}
{{end}}

{{range .model.Responses}}{{template "decode" .}}
{{end}}

{{define "response"}}
#[derive(Clone, Debug)]
pub struct {{CamelCase .Name}} { {{range .Fields}}
    pub {{snakeCase .Name}}: {{template "type" .Type}},{{end}}
}
{{end}}

{{define "decode"}}
pub fn {{snakeCase .Name}}(packet: &[u8; 64]) -> Result<{{CamelCase .Name}}, Box<dyn Error>> {
    if packet.len() != 64 {
        return Err(format!("invalid reply packet length ({})", packet.len()))?;
    }
 
    if packet[1] != 0x94 {
        return Err(format!("invalid reply function code ({:02x})", packet[1]))?;
    }
 
    let response = {{CamelCase .Name}}{ {{range .Fields}}
        {{snakeCase .Name}}: unpack_{{snakeCase .Type}}(packet, {{.Offset}}),{{end}}
    };

    return Ok(response);
}
{{end}}

fn unpack_uint32(packet: &[u8; 64], offset: usize) -> u32 {
    let mut bytes: [u8; 4] = [0; 4];

    bytes.clone_from_slice(&packet[offset..offset + 4]);

    return u32::from_le_bytes(bytes);
}

fn unpack_ipv4(packet: &[u8; 64], offset: usize) -> Ipv4Addr {
    let u0 = packet[offset];
    let u1 = packet[offset + 1];
    let u2 = packet[offset + 2];
    let u3 = packet[offset + 3];

    return Ipv4Addr::new(u0, u1, u2, u3);
}

fn unpack_mac(packet: &[u8; 64], offset: usize) -> [u8; 6] {
    let mut mac: [u8; 6] = [0; 6];

    mac.clone_from_slice(&packet[offset..offset + 6]);

    return mac;
}

fn unpack_version(packet: &[u8; 64], offset: usize) -> String {
    let major = packet[offset];
    let minor = packet[offset + 1];

    return format!("v{major:x}.{minor:02x}").to_string();
}

fn unpack_date(packet: &[u8; 64], offset: usize) -> NaiveDate {
    let ix: [usize; 8] = [
        (packet[offset] >> 4 & 0x0f).into(),
        (packet[offset] >> 0 & 0x0f).into(),
        (packet[offset + 1] >> 4 & 0x0f).into(),
        (packet[offset + 1] >> 0 & 0x0f).into(),
        (packet[offset + 2] >> 4 & 0x0f).into(),
        (packet[offset + 2] >> 0 & 0x0f).into(),
        (packet[offset + 3] >> 4 & 0x0f).into(),
        (packet[offset + 3] >> 0 & 0x0f).into(),
    ];

    let chars = [
        BCD[ix[0]], BCD[ix[1]], BCD[ix[2]], BCD[ix[3]], BCD[ix[4]], BCD[ix[5]], BCD[ix[6]],
        BCD[ix[7]],
    ];

    let d = String::from_iter(chars);

    match NaiveDate::parse_from_str(&d, "%Y%m%d") {
        Ok(date) => return date,
        Err(_) => return NaiveDate::default()
    }
}

const BCD: [char; 10] = ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9'];
