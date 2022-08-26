import datetime
import struct

from ipaddress import IPv4Address
from dataclasses import dataclass


{{range .model.responses}}{{template "decode" .}}
{{end}}

{{range .model.responses}}{{template "response" .}}
{{end}}

{{define "decode"}}
def {{snakeCase .name}}(packet):
    if len(packet) != 64:
        raise ValueError(f'invalid reply packet length ({len(packet)})')

    if packet[1] != {{byte2hex .msgtype}}:
        raise ValueError(f'invalid reply function code ({packet[1]:02x})')

    return {{CamelCase .name}}({{range .fields}}
        unpack_{{.type}}(packet, {{.offset}}),{{end}}
    )
{{end}}

{{define "response"}}
@dataclass
class {{CamelCase .name}}:{{range .fields}}
    {{snakeCase .name}}: {{template "type" .type}}{{end}}
{{end}}

def unpack_uint8(packet, offset):
    return packet[offset]

def unpack_uint32(packet, offset):
    return struct.unpack_from('<L', packet, offset)[0]

def unpack_IPv4(packet, offset):
    return IPv4Address(packet[offset:offset + 4])

def unpack_MAC(packet, offset):
    return f'{packet[offset]:02x}:{packet[offset+1]:02x}:{packet[offset+2]:02x}:{packet[offset+3]:02x}::{packet[offset+4]:02x}::{packet[offset+5]:02x}'

def unpack_version(packet, offset):
    return f'v{packet[offset]:x}.{packet[offset+1]:02x}'

def unpack_date(packet, offset):
    bcd = f'{packet[offset]:02x}{packet[offset+1]:02x}{packet[offset+2]:02x}{packet[offset+3]:02x}'

    return datetime.datetime.strptime(bcd, '%Y%m%d').date()

def unpack_bool(packet, offset):
    return packet[offset] != 0x00

def unpack_HHmm(packet, offset):
    bcd = f'{packet[offset]:02x}{packet[offset+1]:02x}'

    return datetime.datetime.strptime(bcd, '%H%M').time()

