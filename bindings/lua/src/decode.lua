local decode = {}
local structs = require("src/structs")

{{range .model.responses}}
{{- template "decode" . -}}
{{end}}

{{- with .model.event}}
{{- template "decode" . -}}
{{end}}

{{define "decode"}}
function decode.{{snakeCase .name}}(packet)
    if #packet ~= 64 then
        error("invalid reply packet length (" .. #packet .. ")")
    end

    --  Ref. v6.62 firmware event
    if string.byte(packet, 1) ~= 0x17 and (string.byte(packet, 1) ~= 0x19 or string.byte(packet, 2) ~= 0x20) then
        error(string.format("invalid reply start of message byte (%02x)", string.byte(packet, 1)))
    end

    if string.byte(packet,2) ~= {{byte2hex .msgtype}} then
        error(string.format("invalid reply function code (%02x)", string.byte(packet,2)))
    end

    {{range .fields}}
    local {{snakeCase .name}} = unpack_{{snakeCase .type}}(packet, {{.offset}})
    {{- end}}

    return structs.{{snakeCase .name}}({{- template "construct" .fields -}})
end
{{end}}

function unpack_uint8(packet, offset)
    return string.unpack("B", packet, offset + 1)
end

function unpack_uint16(packet, offset)
    return string.unpack("<I2", packet, offset + 1)
end

function unpack_uint32(packet, offset)
    return string.unpack("<I4", packet, offset + 1)
end

function unpack_bool(packet, offset)
    return string.unpack("B", packet, offset + 1) == 0x01
end

function unpack_ipv4(packet, offset)
    local address = { string.unpack("BBBB", packet, offset + 1) }

    return string.format("%u.%u.%u.%u", address[1], address[2], address[3], address[4])
end

function unpack_mac(packet, offset)
    local MAC = { string.unpack("BBBBBB", packet, offset + 1) }

    return string.format("%02x:%02x:%02x:%02x:%02x:%02x", MAC[1], MAC[2], MAC[3], MAC[4], MAC[5], MAC[6])
end

function unpack_version(packet, offset)
    local version = { string.unpack("BB", packet, offset + 1) }
    local major = version[1]
    local minor = version[2]

    return string.format("v%x.%02x", major, minor)
end

function unpack_datetime(packet, offset)
    local bcd = bcd2string(packet:sub(offset + 1, offset + 7))
    local year = tonumber(bcd:sub(1, 4))
    local month = tonumber(bcd:sub(5, 6))
    local day = tonumber(bcd:sub(7, 8))
    local hour = tonumber(bcd:sub(9, 10))
    local minute = tonumber(bcd:sub(11, 12))
    local second = tonumber(bcd:sub(13, 14))

    return string.format("%04u-%02u-%02u %02u:%02u:%02u", year, month, day, hour, minute, second)
end

function unpack_optional_datetime(packet, offset)
    local bcd = bcd2string(packet:sub(offset + 1, offset + 7))
    local year = tonumber(bcd:sub(1, 4))
    local month = tonumber(bcd:sub(5, 6))
    local day = tonumber(bcd:sub(7, 8))
    local hour = tonumber(bcd:sub(9, 10))
    local minute = tonumber(bcd:sub(11, 12))
    local second = tonumber(bcd:sub(13, 14))

    if
        year > 0
        and month > 0
        and day > 0
        and hour >= 0
        and hour < 24
        and minute >= 0
        and minute < 60
        and second >= 0
        and second < 60
    then
        return string.format("%04u-%02u-%02u %02u:%02u:%02u", year, month, day, hour, minute, second)
    else
        return ""
    end
end

function unpack_date(packet, offset)
    local bcd = bcd2string(packet:sub(offset + 1, offset + 4))
    local year = tonumber(bcd:sub(1, 4))
    local month = tonumber(bcd:sub(5, 6))
    local day = tonumber(bcd:sub(7, 8))

    return string.format("%04u-%02u-%02u", year, month, day)
end

function unpack_shortdate(packet, offset)
    local bcd = bcd2string(packet:sub(offset + 1, offset + 3))
    local century = 2000
    local year = tonumber(bcd:sub(1, 2))
    local month = tonumber(bcd:sub(3, 4))
    local day = tonumber(bcd:sub(5, 6))

    return string.format("%04u-%02u-%02u", century + year, month, day)
end

function unpack_optional_date(packet, offset)
    local bcd = bcd2string(packet:sub(offset + 1, offset + 4))
    local year = tonumber(bcd:sub(1, 4))
    local month = tonumber(bcd:sub(5, 6))
    local day = tonumber(bcd:sub(7, 8))

    if year > 0 and month > 0 and day > 0 then
        return string.format("%04u-%02u-%02u", year, month, day)
    else
        return ""
    end
end

function unpack_time(packet, offset)
    local bcd = bcd2string(packet:sub(offset + 1, offset + 3))
    local hour = tonumber(bcd:sub(1, 2))
    local minute = tonumber(bcd:sub(3, 4))
    local second = tonumber(bcd:sub(5, 6))

    return string.format("%02u:%02u:%02u", hour, minute, second)
end

function unpack_hhmm(packet, offset)
    local bcd = bcd2string(packet:sub(offset + 1, offset + 2))
    local hour = tonumber(bcd:sub(1, 2))
    local minute = tonumber(bcd:sub(3, 4))

    return string.format("%02u:%02u", hour, minute)
end

function unpack_pin(packet, offset)
    return string.unpack("<I3", packet, offset + 1)
end

function unpack_mode(packet, offset)
    return string.unpack("B", packet, offset + 1)
end

function unpack_anti_passback(packet, offset)
    return string.unpack("B", packet, offset + 1)
end

function unpack_event_type(packet, offset)
    return string.unpack("B", packet, offset + 1)
end

function unpack_direction(packet, offset)
    return string.unpack("B", packet, offset + 1)
end

function unpack_reason(packet, offset)
    return string.unpack("B", packet, offset + 1)
end

function bcd2string(bytes)
    return bytes:gsub(".", function(c)
        return string.format("%02x", string.byte(c))
    end)
end

return decode
