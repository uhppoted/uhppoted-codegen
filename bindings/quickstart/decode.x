{{range .model.responses}}
{{- template "response" . -}}
{{end}}

{{- with .model.event}}
{{- template "response" . -}}
{{end}}

{{- range .model.responses}}
{{- template "decode" . -}}
{{end}}

{{- with .model.event}}
{{- template "decode" .}}
{{end}}

{{define "response"}}
type {{snakeCase .name}} { {{range .fields}}
    {{snakeCase .name | printf "%-20v"}} {{template "type" .type}}{{end}}
}
{{end}}

{{- define "decode" -}}
func {{snakeCase .name}}(packet []byte) {
    if len(packet) != 64 {
        return error('invalid reply packet length')
    }

    // Ref. v6.62 firmware event
    if packet[0] != 0x17 && (packet[0] != 0x19 || packet[1] != 0x20) {
        return error('invalid reply start of message byte')    
    }

    if packet[1] != {{byte2hex .msgtype}} {
        return error('invalid reply function code')
    }

    return {{snakeCase .name}}{ {{range .fields}}
        {{snakeCase .name | printf "%v:" | printf "%-21v" }} unpack{{CamelCase .type}}(packet, {{.offset}}),
    {{- end}}
    }
}
{{- end -}}

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

func unpackMode(packet, offset) {
    return packet[offset]
}

func bcd2string(bytes) {
    return hex::encode(bytes)
}

