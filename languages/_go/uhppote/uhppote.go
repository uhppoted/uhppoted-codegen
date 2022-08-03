package uhppote

import(
    "fmt"
)

{{range .Functions}}{{template "function" .}}
{{end}}

{{define "function"}}
func {{CamelCase .Name}}({{template "args" .Args}}) error {
    fmt.Printf(">> {{.Name}}\n")

    request,err := {{CamelCase .Name}}Request({{template "params" .Args}})
    if err != nil {
        return err
    }

    fmt.Printf(">> %v\n", request)

    return nil
}
{{end}}

{{define "args"}}{{range .}}{{camelCase .Name}} {{template "type" .Type}}{{end}}{{end}}

{{define "params"}}{{range .}}{{camelCase .Name}}{{end}}{{end}}

{{define "type"}}{{if eq . "uint32"}}uint32{{else}}any{{end}}{{end}}

