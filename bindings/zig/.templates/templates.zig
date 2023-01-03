{{define "args"}}{{range $index, $arg := .}}{{if ne .type "magic"}}{{if $index}}, {{end}}{{snakeCase .name}}: {{template "type" .type}}{{end}}{{end}}{{end}}

{{define "type"}}{{lookup "zig.types" . "???"}}{{end}}
