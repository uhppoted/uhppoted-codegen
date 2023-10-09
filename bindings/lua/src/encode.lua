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

function pack_uint32(v,packet,offset)
    bytes = string.pack("<I4", v)
    
    packet[offset+1] = string.byte(bytes,1)
    packet[offset+2] = string.byte(bytes,2)
    packet[offset+3] = string.byte(bytes,3)
    packet[offset+4] = string.byte(bytes,4)
   
    return packet
end

function pack_IPv4(v,packet,offset)
    local a,b,c,d = string.match(v,"^([0-9]+)%.([0-9]+)%.([0-9]+)%.([0-9]+)$")

    a = tonumber(a)
    b = tonumber(b)
    c = tonumber(c)
    d = tonumber(d)

    if a < 0 or a > 255 or b < 0 or b > 255 or c < 0 or c > 255 or d < 0 or d > 255 then
      error("invalid IPv4 address (" .. v .. ")")
    end
    
    packet[offset+1] = a
    packet[offset+2] = b
    packet[offset+3] = c
    packet[offset+4] = d
   
    return packet
end

return encode