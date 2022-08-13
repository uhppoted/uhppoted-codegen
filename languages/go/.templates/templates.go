{{define "args"}}{{range .}}{{camelCase .Name}} {{template "type" .Type}}{{end}}{{end}}
{{define "params"}}{{range .}}{{camelCase .Name}}{{end}}{{end}}
{{define "type"}}{{lookup "go.types" . "???"}}{{end}}
