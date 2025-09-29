package uhppote

import(
    "fmt"
    "encoding/binary"
    "net/netip"
)

{{range .model.requests}}
{{- template "request" . -}}
{{end}}

{{define "request"}}
func {{CamelCase .name}}({{template "args" .fields}}) ([]byte,error) {
    packet := make([]byte,64)

    packet[0] = 0x17
    packet[1] = {{byte2hex .msgtype}}
    {{range .fields -}}
    {{if ne .type "magic"}}
    if err := pack{{CamelCase .type}}({{camelCase .name}}, packet, {{.offset}}); err != nil {
        return nil, err
    }
    {{else}}
    if err := packUint32(0x55aaaa55, packet, {{.offset}}); err != nil {
        return nil, err        
    }
    {{end}}{{end}}
    return packet, nil
}
{{end}}

func packUint8(v uint8, packet []byte, offset uint8) error {
    packet[offset] = v

    return nil
}

func packUint16(v uint16, packet []byte, offset uint8) error {
    binary.LittleEndian.PutUint16(packet[offset:offset+2], v)

    return nil
}

func packUint32(v uint32, packet []byte, offset uint8) error {
    binary.LittleEndian.PutUint32(packet[offset:offset+4], v)

    return nil
}

func packIPv4(v netip.Addr, packet []byte, offset uint8) error {
    addr := v.As4()
    copy(packet[offset:], addr[:])

    return nil
}

func packAddressPort(v netip.AddrPort, packet []byte, offset uint8) error {
    addr := v.Addr().As4()
    port := v.Port()

    copy(packet[offset:], addr[:])
    binary.LittleEndian.PutUint16(packet[offset+4:offset+6], port)

    return nil
}

func packDate(v Date, packet []byte, offset uint8) error {
    s := v.Format("20060102")
    
    if bytes, err := string2bcd(s); err != nil {
        return err
    } else {
        copy(packet[offset:], bytes)

        return nil        
    }
}

func packDatetime(v DateTime, packet []byte, offset uint8) error {
    s := v.Format("20060102150405")
    
    if bytes, err := string2bcd(s); err != nil {
        return err
    } else {
        copy(packet[offset:], bytes)

        return nil        
    }
}

func packHHmm(v HHmm, packet []byte, offset uint8) error {
    s := v.Format("1504")
    
    if bytes, err := string2bcd(s); err != nil {
        return err
    } else {
        copy(packet[offset:], bytes)

        return nil        
    }
}

func packBool(v bool, packet []byte, offset uint8) error {
    if v {
        packet[offset] = 0x01
    } else {
        packet[offset] = 0x00
    }

    return nil
}

func packPin(v PIN, packet []byte, offset uint8) error {
    bytes := make([]byte, 4)
    binary.LittleEndian.PutUint32(bytes, uint32(v))

    packet[offset] = bytes[0]
    packet[offset+1] = bytes[1]
    packet[offset+2] = bytes[2]

    return nil
}

func packTask(v uint8, packet []byte, offset uint8) error {
    packet[offset] = v

    return nil
}

func packInterlock(v uint8, packet []byte, offset uint8) error {
    packet[offset] = v

    return nil
}

func string2bcd(s string) ([]byte, error) {
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
            return nil, fmt.Errorf("invalid BCD string %v", s)
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

    return bytes, nil
}

