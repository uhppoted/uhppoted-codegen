{{define "args"}}{{range .}}{{snakeCase .Name}}: {{template "type" .Type}}{{end}}{{end}}
{{define "params"}}{{range .}}{{snakeCase .Name}}{{end}}{{end}}
{{define "type"}}{{lookup "rust.types" . "???"}}{{end}}
