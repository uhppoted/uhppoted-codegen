package uhppote

import(
    "reflect"
    "testing"
)

{{range .testdata.tests}}
{{- if .response}}{{template "test" .}}{{end -}}
{{end}}

{{define "test"}}
func Test{{CamelCase .name}}Response(t *testing.T) {
    expected := {{CamelCase .response.name}}{
        {{range .response.values -}}
        {{CamelCase .name}}: {{template "var" .}},
        {{end}}
    }

    reply := []uint8{
       {{dump .response.message "        "}},
    }

    response,err := {{camelCase .response.name}}(reply)
    if err != nil {
        t.Fatalf("unexpected error (%v)", err)
    } else if response == nil {
        t.Fatalf("no response")        
    }

   if !reflect.DeepEqual(*response, expected) {
       t.Errorf("incorrectly encoded {{kebabCase .name }} response\n   expected:%+v\n   got:     %+v\n", expected, *response)
   }
}
{{end}}

