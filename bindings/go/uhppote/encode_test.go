package uhppote

import(
    "reflect"
    "testing"
)

{{range .testdata.tests}}
{{- template "test" . -}}
{{end}}

{{define "test"}}
func Test{{CamelCase .name}}Request(t *testing.T) {
   expected := []uint8{
    {{ dump .request.message "        "}},
   }

   request,err := {{CamelCase .request.name}}({{template "values" .request.values}})
   if err != nil {
       t.Fatalf("unexpected error (%v)", err)
   }

   if !reflect.DeepEqual(request, expected) {
       t.Errorf("incorrectly encoded {{kebabCase .name }} request\n   expected:%v\n   got:     %v\n", format(expected), format(request))
   }
}
{{end}}

