{{range .model.responses}}
{{ template "response" . -}}
{{end}}

{{define "response"}}
-record({{snakeCase .name}}, {
    {{- range (subslice .fields)}}
    {{snakeCase .name}},{{end}}
    {{range (last .fields)}}{{snakeCase .name}}{{end}}
}).
{{- end}}

{{with .model.event}}
-record({{snakeCase .name}}, {
    {{- range (subslice .fields)}}
    {{snakeCase .name}},{{end}}
    {{range (last .fields)}}{{snakeCase .name}}{{end}}
}).
{{end}}

