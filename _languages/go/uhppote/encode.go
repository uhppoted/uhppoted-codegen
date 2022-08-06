package uhppote

import(
    "encoding/binary"
)

{{range .Requests}}{{template "request" .}}
{{end}}

{{define "request"}}
func {{CamelCase .Name}}({{template "args" .Fields}}) ([]byte,error) {
    packet := make([]byte,64)

    packet[0] = 0x17
    packet[1] = {{printf "0x%02x" .MsgType}}

    {{range .Fields}}
    pack{{CamelCase .Type}}(packet, {{camelCase .Name}}, {{.Offset}})
    {{end}}

    return packet, nil
}
{{end}}

{{define "args"}}{{range .}}{{camelCase .Name}} {{template "type" .Type}}{{end}}{{end}}

{{define "type"}}{{if eq . "uint32"}}uint32{{else}}any{{end}}{{end}}

func packUint32(packet []byte, v uint32, offset uint8) {
    binary.LittleEndian.PutUint32(packet[offset:offset+4], v)
}