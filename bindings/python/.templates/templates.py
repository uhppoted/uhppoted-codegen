{{define "args"}}{{range $index, $arg := .}}{{if ne .type "magic"}}{{if $index}}, {{end}}{{snakeCase .name}}{{end}}{{end}}{{end}}

{{define "params"}}{{range $index, $param := .}}{{if ne .type "magic"}}{{if $index}}, {{end}}{{snakeCase .name}}{{end}}{{end}}{{end}}

{{define "type"}}{{lookup "python.types" . "???"}}{{end}}
