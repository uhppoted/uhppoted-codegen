use std::error::Error;

{{range .model.Requests}}{{template "request" .}}
{{end}}

{{define "request"}}
pub fn {{snakeCase .Name}}({{template "args" .Fields}}) -> Result<[u8; 64], Box<dyn Error>> {
    let mut packet = [0x00; 64];

    packet[0] = 0x17;
    packet[1] = {{printf "0x%02x" .MsgType}};

    {{range .Fields}}
    pack_{{template "type" .Type}}(&mut packet, {{snakeCase .Name}}, {{.Offset}});
    {{end}}

    return Ok(packet)
}
{{end}}


fn pack_u32(packet: &mut [u8; 64], v: u32, offset: usize) {
    let bytes = v.to_le_bytes();

    packet[offset..offset + 4].clone_from_slice(&bytes);
}

{{define "args"}}{{range .}}{{snakeCase .Name}}: {{template "type" .Type}}{{end}}{{end}}
{{define "type"}}{{lookup "rust.types" . "???"}}{{end}}
