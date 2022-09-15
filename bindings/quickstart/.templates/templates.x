{{define "args"}}{{ range $index, $arg := .}}{{if ne .type "magic"}}{{if $index}}, {{end}}{{snakeCase .name}} {{template "type" .type}}{{end}}{{end}}{{end}}

{{define "params"}}{{ range $index, $param := .}}{{if ne .type "magic"}}{{if $index}}, {{end}}{{snakeCase .name}}{{end}}{{end}}{{end}}

{{define "values"}}{{ range $index, $param := .}}{{if $index}}, {{end}}{{template "var" .}}{{end}}{{end}}

{{define "type"}}{{lookup "x.types" . "???"}}{{end}}

{{define "var"}}to{{CamelCase .type}}("{{ .value }}"){{end}}




