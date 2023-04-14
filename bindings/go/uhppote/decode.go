package uhppote

import (
    "encoding/binary"
    "encoding/hex"
    "fmt"
    "net/netip"
    "regexp"
    "time"
)

{{range .model.responses}}
{{- template "response" . -}}
{{end}}

{{with .model.event}}
type {{CamelCase .name}} struct { {{range .fields}}
    {{CamelCase .name}} {{template "type" .type}} `json:"{{kebabCase .name}}"`{{end}}
}
{{end}}

{{range .model.responses}}
{{- template "decode" . -}}
{{end}}

{{with .model.event}}{{template "decode" .}}
{{end}}

{{define "decode"}}
func {{camelCase .name}}(packet []byte) (response *{{CamelCase .name}}, err error) {
    if len(packet) != 64 {
        err = fmt.Errorf("invalid reply packet length (%v)", len(packet))
        return
    }

    // Ref. v6.62 firmware event
    if packet[0] != 0x17 && (packet[0] != 0x19 || packet[1] != 0x20) {
        err = fmt.Errorf("invalid reply start of message byte (%02x)", packet[0])
        return
    }

    if packet[1] != {{byte2hex .msgtype}} {
        err = fmt.Errorf("invalid reply function code (%02x)", packet[1])
        return
    }

    v := {{CamelCase .name}}{}
    {{range .fields}}
    if v.{{CamelCase .name}},err = unpack{{CamelCase .type}}(packet, {{.offset}}); err != nil {
        return
    }
    {{end}}
    response = &v

    return
}
{{end}}

func unpackUint8(packet []byte, offset uint8) (uint8, error) {
    return packet[offset], nil
}

func unpackUint16(packet []byte, offset uint8) (uint16, error) {
    return binary.LittleEndian.Uint16(packet[offset:offset+2]), nil
}

func unpackUint32(packet []byte, offset uint8) (uint32, error) {
    return binary.LittleEndian.Uint32(packet[offset:offset+4]), nil
}

func unpackBool(packet []byte, offset uint8) (bool, error) {
    return packet[offset] != 0x00, nil
}

func unpackIPv4(packet []byte, offset uint8) (netip.Addr, error) {
    addr,_ := netip.AddrFromSlice(packet[offset:offset+4])

    return addr, nil
}

func unpackMAC(packet []byte, offset uint8) (string, error) {
    mac := fmt.Sprintf("%02x:%02x:%02x:%02x:%02x:%02x", 
        packet[offset],
        packet[offset+1],
        packet[offset+2],
        packet[offset+3],
        packet[offset+4],
        packet[offset+5])

    return mac, nil
}

func unpackVersion(packet []byte, offset uint8) (string, error) {
    major := packet[offset]
    minor := packet[offset+1]

    return fmt.Sprintf("v%x.%02x", major, minor), nil
}

func unpackDate(packet []byte, offset uint8) (Date, error) {
    bcd := bcd2string(packet[offset:offset+4])

    if date, err := time.ParseInLocation("20060102", bcd, time.Local); err != nil {
        return Date(time.Time{}), err
    } else {
        return Date(date), nil
    }
}

func unpackShortdate(packet []byte, offset uint8) (Date, error) {
    bcd := "20" + bcd2string(packet[offset:offset+3])

    if date, err := time.ParseInLocation("20060102", bcd, time.Local); err != nil {
        return Date(time.Time{}), err
    } else {
        return Date(date), nil
    }
}

func unpackOptionalDate(packet []byte, offset uint8) (Date, error) {
    bcd := bcd2string(packet[offset:offset+4])

    if d, err := time.ParseInLocation("20060102", bcd, time.Local); err != nil {
        return Date{}, nil
    } else {
        return  Date(d), nil
    }
}

func unpackDatetime(packet []byte, offset uint8) (DateTime, error) {
    bcd := bcd2string(packet[offset:offset+7])

    if date, err := time.ParseInLocation("20060102150405", bcd, time.Local); err != nil {
        return DateTime(time.Time{}), err
    } else {
        return DateTime(date), nil
    }
}

func unpackOptionalDatetime(packet []byte, offset uint8) (DateTime, error) {
    bcd := bcd2string(packet[offset:offset+7])

    if date, err := time.ParseInLocation("20060102150405", bcd, time.Local); err != nil {
        return DateTime{}, err
    } else {
        return DateTime(date), nil
    }
}

func unpackTime(packet []byte, offset uint8) (Time, error) {
    bcd := bcd2string(packet[offset:offset+3])

    if t, err := time.ParseInLocation("150405", bcd, time.Local); err != nil {
        return Time(time.Time{}), err
    } else {
        return Time(t), nil
    }
}

func unpackHHmm(packet []byte, offset uint8) (HHmm, error) {
    bcd := bcd2string(packet[offset:offset+2])

    if t, err := time.ParseInLocation("1504", bcd, time.Local); err != nil {
        return HHmm(time.Time{}), nil
    } else {
        return HHmm(t), nil
    }
}

func unpackPin(packet []byte, offset uint8) (PIN, error) {
    b := []byte { packet[offset+0], packet[offset+1], packet[offset+2], 0x00 }
    v := binary.LittleEndian.Uint32(b)

    return PIN(v), nil
}

func bcd2string(bytes []byte) string {
    BCD := hex.EncodeToString(bytes)

    if matched,err := regexp.MatchString(`^[0-9]*$`, BCD); err != nil || !matched {
        panic(fmt.Sprintf("invalid BCD value (%v)", bytes))        
    } 

    return BCD
}

{{define "response"}}
type {{CamelCase .name}} struct { {{range .fields}}
    {{CamelCase .name}} {{template "type" .type}} `json:"{{kebabCase .name}}"`{{end}}
}
{{end}}

