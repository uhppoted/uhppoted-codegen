package uhppote

import(
    "fmt"
    "reflect"
    "strings"
    "testing"
)

{{with $n := index .testdata.tests 0}}{{template "test" $n}}{{end}}

{{define "test"}}
func Test{{CamelCase .name}}(t *testing.T) {
   request,err := {{CamelCase .name}}Request({{template "values" .request.values}})
   if err != nil {
       t.Fatalf("unexpected error (%v)", err)
   }

   expected := []uint8{
{{ dump .request.message "        "}},
   }

   if !reflect.DeepEqual(request, expected) {
       t.Errorf("incorrectly encoded {{ .name }} request\n   expected:%v\n   got:     %v\n", format(expected), format(request))
   }
}
{{end}}


func format(packet []byte) string {
    var s strings.Builder
    
    hex := "%02x %02x %02x %02x %02x %02x %02x %02x"
    prefix := ""

    for i := 0; i < 4; i++ {
        offset := i * 16
        u := packet[offset : offset+8]
        v := packet[offset+8 : offset+16]

        p := fmt.Sprintf(hex, u[0], u[1], u[2], u[3], u[4], u[5], u[6], u[7])
        q := fmt.Sprintf(hex, v[0], v[1], v[2], v[3], v[4], v[5], v[6], v[7])

        fmt.Fprintf(&s,"%v   %08x  %v  %v\n", prefix, offset, p, q)
        prefix = "            "
    }

    return s.String()
}
