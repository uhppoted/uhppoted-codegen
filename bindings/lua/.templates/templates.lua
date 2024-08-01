{{define "args"}}{{range $index, $arg := .}}{{if ne .type "magic"}}{{if $index}}, {{end}}{{snakeCase .name}}{{end}}{{end}}{{end}}

{{define "params"}}{{range $index, $param := .}}{{if ne .type "magic"}}, {{snakeCase .name}}{{end}}{{end}}{{end}}

{{define "construct"}}{{range $index, $param := .}}{{if $index}}, {{end}}{{snakeCase .name}}{{end}}{{end}}

