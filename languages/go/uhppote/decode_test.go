package uhppote

import(
    // "fmt"
    // "net/netip"
    "reflect"
    // "strconv"
    // "strings"
    "testing"
    // "time"
)

{{with $n := index .testdata.tests 0}}
{{- template "test" $n -}}
{{end}}

{{define "test"}}
func Test{{CamelCase .name}}Response(t *testing.T) {
    expected := {{CamelCase .name}}Response{
        {{range .response.values -}}
        {{CamelCase .name}}: {{template "var" .}},
        {{end}}
    }

    reply := []uint8{
    {{dump .response.message "        "}},
    }

    response,err := {{camelCase .name}}Response(reply)
    if err != nil {
        t.Fatalf("unexpected error (%v)", err)
    } else if response == nil {
        t.Fatalf("no response")        
    }

   if !reflect.DeepEqual(*response, expected) {
       t.Errorf("incorrectly encoded {{kebabCase .name }} response\n   expected:%#v\n   got:     %#v\n", expected, *response)
   }
}
{{end}}

