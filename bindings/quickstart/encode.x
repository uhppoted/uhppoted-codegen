{{range .model.requests}}
{{- template "request" . -}}
{{end}}

{{- define "request"}}
function {{snakeCase .name}}({{template "args" .fields}}) {
    packet = byte[64]

    packet[0] = 0x17
    packet[1] = {{byte2hex .msgtype}}
    {{range .fields -}}
    {{if ne .type "magic"}}
    pack{{CamelCase .type}}({{camelCase .name}}, packet, {{.offset}})
    {{- else}}
    packUint32(0x55aaaa55, packet, {{.offset}})
    {{- end}}{{end}}

    return packet, None
}
{{end}}

function packUint8(v, packet, offset) {
    packet[offset] = v
}

function packUint16(v, packet, offset) {
    packet[offset:offset+2] = uint16::little_endian(v)
}

function packUint32(v, packet, offset) {
    packet[offset:offset+4] = uint32::little_endian(v)
}

function packIPv4(v, packet, offset) {
    packet[offset:offset+4] = IPv4::as_bytes(v)
}

function packDate(v, packet, offset) {
    s   = datetime::format('YYYYMMDD',v)
    bcd = string2bcd(s)

    packet[offset:offset+4] = bcd
}

function packDatetime(v, packet, offset) {
    s   = datetime::format('YYYYMMDDHHmmss',v)
    bcd = string2bcd(s)

    packet[offset:offset+7] = bcd
}

function packHHmm(v, packet, offset) {
    s   = datetime::format('HHmm',v)
    bcd = string2bcd(s)

    packet[offset:offset+2] = bcd
}

function packBool(v bool, packet, offset) {
    if v {
        packet[offset] = 0x01
    } else {
        packet[offset] = 0x00
    }
}

function string2bcd(s string) ([]byte, error) {
    return s.window(2).map(v => parse_hex(v))
}

