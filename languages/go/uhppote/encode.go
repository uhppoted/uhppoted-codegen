package uhppote

import(
    "fmt"
    "encoding/binary"
    "net/netip"
)

{{range .model.requests}}{{template "request" .}}
{{end}}

{{define "request"}}
func {{CamelCase .name}}({{template "args" .fields}}) ([]byte,error) {
    packet := make([]byte,64)

    packet[0] = 0x17
    packet[1] = {{byte2hex .msgtype}}
    {{range .fields}}
    {{if ne .type "magic"}}pack{{CamelCase .type}}(packet, {{camelCase .name}}, {{.offset}}){{else}}packUint32(packet, 0x55aaaa55, {{.offset}})
    {{end}}{{end}}

    return packet, nil
}
{{end}}

func packUint16(packet []byte, v uint16, offset uint8) {
    binary.LittleEndian.PutUint16(packet[offset:offset+2], v)
}

func packUint32(packet []byte, v uint32, offset uint8) {
    binary.LittleEndian.PutUint32(packet[offset:offset+4], v)
}

func packIPv4(packet []byte, v netip.Addr, offset uint8) {
    addr := v.As4()
    copy(packet[offset:], addr[:])
}

func packDatetime(packet []byte, v DateTime, offset uint8) {
    s := v.Format("20060102150405")
    bytes := string2bcd(s)

    copy(packet[offset:], bytes)
}

func string2bcd(s string) []byte {
    BCD := map[rune]uint8 {
        '0': 0x00,
        '1': 0x01,
        '2': 0x02,
        '3': 0x03,
        '4': 0x04,
        '5': 0x05,
        '6': 0x06,
        '7': 0x07,
        '8': 0x08,
        '9': 0x09,
    }

    nibbles := []byte{}

    if len(s) %2 != 0 {
        nibbles = append(nibbles,0)
    }

    for _,ch := range s {        
        if n,ok := BCD[ch]; !ok {
            panic(fmt.Sprintf("Invalid BCD digit (%v)", ch))
        } else {
            nibbles = append(nibbles, n)
        }
    }

    bytes := []byte{}
    for i := 0; i<len(nibbles); i+=2 {
        msb := nibbles[i]
        lsb := nibbles[i+1]
        b := (msb << 4) | lsb

        bytes = append(bytes,b)
    }

    return bytes
}

