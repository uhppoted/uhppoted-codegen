{{define "args"}}{{ range $index, $arg := .}}{{if ne .type "magic"}}{{if $index}}, {{end}}{{camelCase .name}} {{template "type" .type}}{{end}}{{end}}{{end}}

{{define "params"}}{{ range $index, $param := .}}{{if ne .type "magic"}}{{if $index}}, {{end}}{{camelCase .name}}{{end}}{{end}}{{end}}

{{define "type"}}{{lookup "go.types" . "???"}}{{end}}

{{define "values"}}{{ range $index, $param := .}}{{if $index}}, {{end}}{{.value}}{{end}}{{end}}



