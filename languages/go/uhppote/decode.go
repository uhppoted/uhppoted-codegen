package uhppote

import (
    "encoding/binary"
    "fmt"
    "net/netip"
    "strings"
    "time"
)

{{range .model.Responses}}{{template "decode" .}}
{{end}}

{{range .model.Responses}}{{template "response" .}}
{{end}}

{{define "decode"}}
func {{camelCase .Name}}(packet []byte) (*{{CamelCase .Name}}, error) {
    if len(packet) != 64 {
        return nil, fmt.Errorf("invalid reply packet length (%v)", len(packet))
    }

    if packet[1] != {{printf "0x%02x" .MsgType}} {
        return nil, fmt.Errorf("invalid reply function code (%02x)", packet[1])
    }

    response := {{CamelCase .Name}}{ {{range .Fields}}
        {{CamelCase .Name}}: unpack{{CamelCase .Type}}(packet, {{.Offset}}),{{end}}
    }

    return &response, nil
}
{{end}}

func unpackUint32(packet []byte, offset uint8) uint32 {
    return binary.LittleEndian.Uint32(packet[offset:offset+4])
}

func unpackIPv4(packet []byte, offset uint8) netip.Addr {
    addr,_ := netip.AddrFromSlice(packet[offset:offset+4])

    return addr
}

func unpackMAC(packet []byte, offset uint8) MAC {
    mac := make([]byte,6)

    copy(mac,packet[offset : offset+6])
    
    return MAC(mac)
}

func unpackVersion(packet []byte, offset uint8) string {
    major := packet[offset]
    minor := packet[offset+1]

    return fmt.Sprintf("v%x.%02x", major, minor)
}

func unpackDate(packet []byte, offset uint8) Date {
    if bcd, err := bcd2string(packet[offset:offset+4]); err != nil {
        return Date(time.Time{})
    } else if date, err := time.ParseInLocation("20060102", bcd, time.Local); err != nil {
        return Date(time.Time{})
    } else {
        return Date(date)
    }
}

var BCD = map[uint8]rune {
    0x00: '0',
    0x01: '1',
    0x02: '2',
    0x03: '3',
    0x04: '4',
    0x05: '5',
    0x06: '6',
    0x07: '7',
    0x08: '8',
    0x09: '9',
}

func bcd2string(bytes []byte) (string, error) {
    var s strings.Builder

    s.Grow(len(bytes) * 2)

    for _, b := range bytes {
        if v,ok := BCD[(b >> 4) & 0x0f]; ok {
            s.WriteRune(v)
        } else {
            return "", fmt.Errorf("Invalid BCD number: '%x'", bytes)
        }

        if v,ok := BCD[b & 0x0f]; ok {
            s.WriteRune(v)
        } else {
            return "", fmt.Errorf("Invalid BCD number: '%x'", bytes)
        }
    }

    return s.String(), nil
}


{{define "response"}}
type {{CamelCase .Name}} struct { {{range .Fields}}
    {{CamelCase .Name}} {{template "type" .Type}} `json:"{{kebabCase .Name}}"`{{end}}
}
{{end}}

{{define "type"}}{{lookup "go.types" . "???"}}{{end}}

