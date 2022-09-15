{{range .model.responses}}
{{- template "response" . -}}
{{end}}

{{with .model.event}}
type {{snakeCase .name}} struct { {{range .fields}}
    {{snakeCase .name}} {{template "type" .type}} {{end}}
}
{{end}}

{{range .model.responses}}
{{- template "decode" . -}}
{{end}}

{{with .model.event}}{{template "decode" .}}
{{end}}

{{define "decode"}}
func {{snakeCase .name}}(packet []byte) {
    if len(packet) != 64 {
        return error('invalid reply packet length')
    }

    if packet[1] != {{byte2hex .msgtype}} {
        return error('invalid reply function code')
    }

    return {{snakeCase .name}}{ {{range .fields}}
        v.{{snakeCase .name}} = unpack{{CamelCase .type}}(packet, {{.offset}}),
    }
    {{end}}

    }
}
{{end}}

func unpackUint8(packet, offset) {
    return packet[offset]
}

func unpackUint16(packet, offset) {
    return Uint16.LittleEndian(packet[offset:offset+2])
}

func unpackUint32(packet, offset) {
    return Uint32.LittleEndian(packet[offset:offset+4])
}

func unpackBool(packet, offset) {
    return packet[offset] != 0x00
}

func unpackIPv4(packet, offset) {
    return IPv4(packet[offset:offset+4])
}

func unpackMAC(packet, offset){
    return hex(packet[offset:offset+5])
}

func unpackVersion(packet, offset) {
    major = packet[offset]
    minor = packet[offset+1]

    return format('v{major}.{minor})
}

func unpackDate(packet, offset) {
    bcd = bcd2string(packet[offset:offset+4])

    return datetime::parse('YYYYMMDD', bcd)
}

func unpackShortdate(packet, offset) {
    bcd = '20' + bcd2string(packet[offset:offset+3])

    return datetime::parse('YYYYMMDD', bcd)
}

func unpackOptionalDate(packet, offset){
    bcd = bcd2string(packet[offset:offset+4])

    return datetime::parse('YYYYMMDD', bcd)
}

func unpackDatetime(packet, offset) s{
    bcd = bcd2string(packet[offset:offset+7])

    return datetime::parse('YYYYMMDDHHmmss', bcd)
}

func unpackOptionalDatetime(packet, offset) {
    bcd = bcd2string(packet[offset:offset+7])

    return datetime::parse('YYYYMMDDHHmmss', bcd)
}

func unpackTime(packet, offset) {
    bcd = bcd2string(packet[offset:offset+3])

    return datetime::parse('HHmmss', bcd)
}

func unpackHHmm(packet, offset) {
    bcd = bcd2string(packet[offset:offset+2])

    return datetime::parse('HHmm', bcd)
}

func bcd2string(bytes) {
    return hex::encode(bytes)
}

{{define "response"}}
type {{snakeCase .name}} struct { {{range .fields}}
    {{snakeCase .name}} {{template "type" .type}}{{end}}
}
{{end}}

