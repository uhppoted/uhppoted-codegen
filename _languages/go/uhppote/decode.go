package uhppote

import (
)

{{range .Responses}}{{template "decode" .}}
{{end}}

{{range .Responses}}{{template "response" .}}
{{end}}

{{define "decode"}}
func {{camelCase .Name}}(packet []byte) (*{{CamelCase .Name}}, error) {
    response := {{CamelCase .Name}}{}

    return &response, nil
}
{{end}}

{{define "response"}}
type {{CamelCase .Name}} struct { {{range .Fields}}
    {{CamelCase .Name}} {{template "type" .Type}} `json:"{{kebabCase .Name}}"`{{end}}
}
{{end}}

{{define "type"}}{{if eq . "uint32"}}uint32{{else}}any{{end}}{{end}}


