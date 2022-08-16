{{define "args"}}{{ range $index, $arg := .}}{{if ne .Type "magic"}}{{if $index}}, {{end}}{{snakeCase .Name}}: {{template "type" .Type}}{{end}}{{end}}{{end}}

{{define "params"}}{{ range $index, $param := .}}{{if ne .Type "magic"}}{{if $index}}, {{end}}{{snakeCase .Name}}{{end}}{{end}}{{end}}

{{define "type"}}{{lookup "rust.types" . "???"}}{{end}}
