import struct

{{range .model.requests}}{{template "request" .}}
{{end}}

{{define "request"}}
def {{snakeCase .name}}({{template "args" .fields}}):
    packet = bytearray(64)

    packet[0] = 0x17
    packet[1] = {{byte2hex .msgtype}}
    
    {{range .fields}}{{if ne .type "magic"}}pack_{{.type}}({{snakeCase .name}}, packet, {{.offset}})
    {{else}}pack_uint32(0x55aaaa55, packet, {{.offset}})
    {{end}}{{end}}

    return packet
{{end}}


def pack_uint8(v, packet, offset):
    packet[offset] = v

def pack_uint16(v, packet, offset):
    struct.pack_into('<H', packet, offset, v)

def pack_uint32(v, packet, offset):
    struct.pack_into('<L', packet, offset, v)

def pack_IPv4(v, packet, offset):
    packet[offset:offset+3] = v.packed
   
def pack_datetime(v, packet, offset):
    bcd = v.strftime('%Y%m%d%H%M%S')
    packet[offset:offset+7] = bytes.fromhex(bcd)

def pack_bool(v, packet, offset):
    packet[offset] = 0x00 if not v else 0x01