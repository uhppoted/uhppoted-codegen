use std::error::Error;
use std::net::Ipv4Addr;

{{range .model.Requests}}{{template "request" .}}
{{end}}

{{define "request"}}
pub fn {{snakeCase .Name}}({{template "args" .Fields}}) -> Result<[u8; 64], Box<dyn Error>> {
    let mut packet = [0x00; 64];

    packet[0] = 0x17;
    packet[1] = {{printf "0x%02x" .MsgType}};

    {{range .Fields}}{{if ne .Type "magic"}}pack_{{snakeCase .Type}}(&mut packet, {{snakeCase .Name}}, {{.Offset}});
    {{else}}pack_uint32(&mut packet, 0x55aaaa55, {{.Offset}});{{end}}{{end}}

    return Ok(packet)
}
{{end}}

fn pack_uint32(packet: &mut [u8; 64], v: u32, offset: usize) {
    let bytes = v.to_le_bytes();

    packet[offset..offset + 4].clone_from_slice(&bytes);
}

fn pack_ipv4(packet: &mut [u8; 64], v: Ipv4Addr, offset: usize) {
    let addr = v.octets();
    
    packet[offset] = addr[0];
    packet[offset+1] = addr[1];
    packet[offset+2] = addr[2];
    packet[offset+3] = addr[3];
}
