package uhppote

import(
    "encoding/binary"
    "net/netip"
)

{{range .model.Requests}}{{template "request" .}}
{{end}}

{{define "request"}}
func {{CamelCase .Name}}({{template "args" .Fields}}) ([]byte,error) {
    packet := make([]byte,64)

    packet[0] = 0x17
    packet[1] = {{printf "0x%02x" .MsgType}}
    {{range .Fields}}
    {{if ne .Type "magic"}}pack{{CamelCase .Type}}(packet, {{camelCase .Name}}, {{.Offset}}){{else}}packUint32(packet, 0x55aaaa55, {{.Offset}})
    {{end}}{{end}}

    return packet, nil
}
{{end}}

func packUint32(packet []byte, v uint32, offset uint8) {
    binary.LittleEndian.PutUint32(packet[offset:offset+4], v)
}

func packIPv4(packet []byte, v netip.Addr, offset uint8) {
    addr := v.As4()
    copy(packet[offset:], addr[:])
}
