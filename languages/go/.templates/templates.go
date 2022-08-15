{{define "args"}}{{ range $index, $arg := .}}{{if ne .Type "magic"}}{{if $index}}, {{end}}{{camelCase .Name}} {{template "type" .Type}}{{end}}{{end}}{{end}}

{{define "params"}}{{ range $index, $param := .}}{{if ne .Type "magic"}}{{if $index}}, {{end}}{{camelCase .Name}}{{end}}{{end}}{{end}}

{{define "type"}}{{lookup "go.types" . "???"}}{{end}}



