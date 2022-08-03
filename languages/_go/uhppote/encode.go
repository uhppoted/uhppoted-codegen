package uhppote

import()

{{range .Requests}}{{template "request" .}}
{{end}}

{{define "request"}}
func {{CamelCase .Name}}Request({{template "args" .Args}}) ([]byte,error) {
    packet := make([]byte,64)

    packet[0] = 0x17
    packet[1] = {{.MsgType}}

    return packet, nil
}
{{end}}

{{define "args"}}{{range .}}{{camelCase .Name}} {{template "type" .Type}}{{end}}{{end}}

{{define "type"}}{{if eq . "uint32"}}uint32{{else}}any{{end}}{{end}}
