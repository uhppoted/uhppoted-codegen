{{define "args"}}{{ range $index, $arg := .}}{{if ne .type "magic"}}{{if $index}}, {{end}}{{CamelCase .name}}{{end}}{{end}}{{end}}

{{define "params"}}{{ range $index, $param := .}}{{if ne .type "magic"}}{{if $index}}, {{end}}{{CamelCase .name}}{{end}}{{end}}{{end}}

{{define "values"}}{{ range $index, $param := .}}{{if $index}}, {{end}}{{template "var" .}}{{end}}{{end}}

{{define "type"}}{{lookup "go.types" . "???"}}{{end}}

{{define "var"}}to{{CamelCase .type}}("{{ .value }}"){{end}}




