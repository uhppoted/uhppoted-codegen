{{define "args"}}{{range $index, $arg := .}}{{if ne .type "magic"}}{{if $index}}, {{end}}{{snakeCase .name}}: {{template "type" .type}}{{end}}{{end}}{{end}}

{{define "params"}}{{range $index, $param := .}}{{if ne .type "magic"}}{{if $index}}, {{end}}{{snakeCase .name}}{{end}}{{end}}{{end}}

{{define "type"}}{{lookup "zig.types" . "???"}}{{end}}

{{define "result"}}{{if .response}}{{CamelCase .response.name}}{{else}}bool{{end}}{{end}}

{{define "var"}}
{{- if eq .type "uint8" "uint16" "pin" "uint32" "bool" "event-type" "direction" "anti-passback"}}{{ .value }}
{{- else -}}
"{{.value}}"
{{- end -}}
{{- end -}}

