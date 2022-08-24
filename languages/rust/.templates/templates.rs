{{define "args"}}{{range $index, $arg := .}}{{if ne .type "magic"}}{{if $index}}, {{end}}{{snakeCase .name}}: {{template "type" .type}}{{end}}{{end}}{{end}}

{{define "params"}}{{range $index, $param := .}}{{if ne .type "magic"}}{{if $index}}, {{end}}{{snakeCase .name}}{{end}}{{end}}{{end}}

{{define "type"}}{{lookup "rust.types" . "???"}}{{end}}

{{define "result"}}{{if .response}}Result<{{CamelCase .response.name}}> {{else}}Result<bool>{{end}}{{end}}