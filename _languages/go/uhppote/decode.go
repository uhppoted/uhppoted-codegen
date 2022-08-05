package uhppote

import (
    "fmt"
)

func decode(packet []byte) (any, error) {
    if len(packet) != 64 {
        return nil, fmt.Errorf("Invalid packet (length:%v byte)", len(packet))
    }

    if packet[0] != 0x17 {
        return nil, fmt.Errorf("Invalid SOM (%02x)", packet[0])
    }

    switch packet[1] { {{range .Responses}}
    case {{printf "0x%02x" .MsgType}}:
        return {{camelCase .Name}}(packet)       
{{end}}

    default:
        return nil, fmt.Errorf("Unknown function code (%02x)", packet[1])
    }

    return nil, nil
}

{{range .Responses}}{{template "decode" .}}
{{end}}

{{range .Responses}}{{template "response" .}}
{{end}}

{{define "decode"}}
func {{camelCase .Name}}(packet []byte) (*{{CamelCase .Name}}, error) {
    return nil, nil
}
{{end}}

{{define "response"}}
type {{CamelCase .Name}} struct { {{range .Fields}}
    {{CamelCase .Name}} {{template "type" .Type}} `json:"{{kebabCase .Name}}"`{{end}}
}
{{end}}

{{define "type"}}{{if eq . "uint32"}}uint32{{else}}any{{end}}{{end}}


