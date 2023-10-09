{{define "args"}}{{range $index, $arg := .}}{{if ne .type "magic"}}{{if $index}}, {{end}}{{snakeCase .name}}{{end}}{{end}}{{end}}
