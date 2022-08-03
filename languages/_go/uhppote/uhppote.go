package uhppote

import(
    "fmt"
)

{{range .Functions}}
func {{CamelCase .Name}}({{template "args" .Args}}) error {
    fmt.Printf(">> {{.Name}}\n")

    return nil
}
{{end}}

{{define "args"}}{{range .}}{{camelCase .Name}} {{template "type" .Type}}{{end}}{{end}}

{{define "type"}}{{if eq . "uint32"}}uint32{{else}}any{{end}}{{end}}
