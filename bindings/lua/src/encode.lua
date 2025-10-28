local encode = {}

{{range .model.requests}}
{{- template "request" . -}}
{{end}}

{{define "request"}}
function encode.{{snakeCase .name}}({{template "args" .fields}})
    packet = make_packet()

    packet[1] = 0x17
    packet[2] = {{byte2hex .msgtype}}
    {{range .fields -}}
    {{if ne .type "magic"}}
    packet = pack_{{snakeCase .type}}({{snakeCase .name}}, packet, {{.offset}})
    {{- else}}
    packet = pack_uint32(0x55aaaa55, packet, {{.offset}})
    {{- end}}{{end}}

    return string.char(table.unpack(packet))
end
{{end}}
-- stylua: ignore start
function make_packet() 
    return { 
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    }
end
-- stylua: ignore end

function pack_bool(v, packet, offset)
    if v then
        packet[offset + 1] = 0x01
    else
        packet[offset + 1] = 0x00
    end

    return packet
end

function pack_uint8(v, packet, offset)
    bytes = string.pack("B", v)

    packet[offset + 1] = string.byte(bytes, 1)

    return packet
end

function pack_uint16(v, packet, offset)
    bytes = string.pack("<I2", v)

    packet[offset + 1] = string.byte(bytes, 1)
    packet[offset + 2] = string.byte(bytes, 2)

    return packet
end

function pack_uint32(v, packet, offset)
    bytes = string.pack("<I4", v)

    packet[offset + 1] = string.byte(bytes, 1)
    packet[offset + 2] = string.byte(bytes, 2)
    packet[offset + 3] = string.byte(bytes, 3)
    packet[offset + 4] = string.byte(bytes, 4)

    return packet
end

function pack_ipv4(v, packet, offset)
    local a, b, c, d = string.match(v, "^([0-9]+)%.([0-9]+)%.([0-9]+)%.([0-9]+)$")

    a = tonumber(a)
    b = tonumber(b)
    c = tonumber(c)
    d = tonumber(d)

    if a < 0 or a > 255 or b < 0 or b > 255 or c < 0 or c > 255 or d < 0 or d > 255 then
        error("invalid IPv4 address (" .. v .. ")")
    end

    packet[offset + 1] = a
    packet[offset + 2] = b
    packet[offset + 3] = c
    packet[offset + 4] = d

    return packet
end

function pack_datetime(v, packet, offset)
    local year, month, day, hour, minute, second = string.match(v, "^(%d%d%d%d)%-(%d%d)%-(%d%d) (%d%d):(%d%d):(%d%d)$")
    local s = string.format("%04d%02d%02d%02d%02d%02d", 
                            tonumber(year), tonumber(month), tonumber(day), 
                            tonumber(hour), tonumber(minute), tonumber(second))

    local bytes = string2bcd(s)
    for i = 1, 7, 1 do
        packet[offset + i] = bytes[i]
    end

    return packet
end

function pack_date(v, packet, offset)
    local year, month, day, hour, minute, second = string.match(v, "^(%d%d%d%d)%-(%d%d)%-(%d%d)$")
    local s = string.format("%04d%02d%02d", tonumber(year), tonumber(month), tonumber(day))
    local bytes = string2bcd(s)

    for i = 1, 4, 1 do
        packet[offset + i] = bytes[i]
    end

    return packet
end

function pack_hhmm(v, packet, offset)
    local hour, minute = string.match(v, "^(%d?%d):(%d%d)$")
    local s = string.format("%02d%02d", tonumber(hour),tonumber(minute))

    local bytes = string2bcd(s)
    for i = 1, 2, 1 do
        packet[offset + i] = bytes[i]
    end

    return packet
end

function pack_pin(v, packet, offset)
    bytes = string.pack("<I3", v)

    packet[offset + 1] = string.byte(bytes, 1)
    packet[offset + 2] = string.byte(bytes, 2)
    packet[offset + 3] = string.byte(bytes, 3)

    return packet
end

function pack_mode(v, packet, offset)
    bytes = string.pack("B", v)

    packet[offset + 1] = string.byte(bytes, 1)

    return packet
end

function pack_task(v, packet, offset)
    bytes = string.pack("B", v)

    packet[offset + 1] = string.byte(bytes, 1)

    return packet
end

function pack_interlock(v, packet, offset)
    bytes = string.pack("B", v)

    packet[offset + 1] = string.byte(bytes, 1)

    return packet
end

function pack_anti_passback(v, packet, offset)
    bytes = string.pack("B", v)

    packet[offset + 1] = string.byte(bytes, 1)

    return packet
end

function string2bcd(s)
    local bytes = {}
    for xx in string.gmatch(s, "(%d%d)") do
        table.insert(bytes, tonumber(xx, 16))
    end

    return bytes
end

return encode
