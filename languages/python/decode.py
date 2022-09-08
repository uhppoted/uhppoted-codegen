import datetime
import struct

from ipaddress import IPv4Address
from dataclasses import dataclass

{{range .model.responses}}
{{- template "decode" . -}}
{{end}}

{{- with .model.event}}
{{- template "decode" . -}}
{{end}}

{{- range .model.responses}}
{{- template "response" . -}}
{{end}}

{{define "decode"}}
def {{snakeCase .name}}(packet):
    if len(packet) != 64:
        raise ValueError(f'invalid reply packet length ({len(packet)})')

    if packet[1] != {{byte2hex .msgtype}}:
        raise ValueError(f'invalid reply function code ({packet[1]:02x})')

    return {{CamelCase .name}}({{range .fields}}
        unpack_{{snakeCase .type}}(packet, {{.offset}}),{{end}}
    )
{{end}}

{{- define "response"}}
@dataclass
class {{CamelCase .name}}:{{range .fields}}
    {{snakeCase .name}}: {{template "type" .type}}{{end}}
{{end}}

{{- with .model.event -}}
@dataclass
class {{CamelCase .name}}:{{range .fields}}
    {{snakeCase .name}}: {{template "type" .type}}{{end}}
{{end}}

def unpack_uint8(packet, offset):
    return packet[offset]

def unpack_uint16(packet, offset):
    return struct.unpack_from('<H', packet, offset)[0]

def unpack_uint32(packet, offset):
    return struct.unpack_from('<L', packet, offset)[0]

def unpack_IPv4(packet, offset):
    return IPv4Address(packet[offset:offset + 4])

def unpack_MAC(packet, offset):
    return '{:02x}:{:02x}:{:02x}:{:02x}::{:02x}:{:02x}'.format(*packet[offset:offset+7])

def unpack_version(packet, offset):
    return 'v{:x}.{:02x}'.format(*packet[offset:offset+2])

def unpack_date(packet, offset):
    bcd = '{:02x}{:02x}{:02x}{:02x}'.format(*packet[offset:offset+4])

    return datetime.datetime.strptime(bcd, '%Y%m%d').date()

def unpack_shortdate(packet, offset):
    bcd = '20{:02x}{:02x}{:02x}'.format(*packet[offset:offset+3])

    return datetime.datetime.strptime(bcd, '%Y%m%d').date()

def unpack_optional_date(packet, offset):
    bcd = '{:02x}{:02x}{:02x}{:02x}'.format(*packet[offset:offset+4])

    try:
        return datetime.datetime.strptime(bcd, '%Y%m%d').date()
    except ValueError as x:
        return None

def unpack_datetime(packet, offset):
    bcd = '{:02x}{:02x}{:02x}{:02x}{:02x}{:02x}{:02x}'.format(*packet[offset:offset+7])

    return datetime.datetime.strptime(bcd, '%Y%m%d%H%M%S')

def unpack_optional_datetime(packet, offset):
    bcd = '{:02x}{:02x}{:02x}{:02x}{:02x}{:02x}{:02x}'.format(*packet[offset:offset+7])

    try:
        return datetime.datetime.strptime(bcd, '%Y%m%d%H%M%S')
    except ValueError as x:
        return None

def unpack_time(packet, offset):
    bcd = '{:02x}{:02x}{:02x}'.format(*packet[offset:offset+3])

    return datetime.datetime.strptime(bcd, '%H%M%S').time()

def unpack_HHmm(packet, offset):
    bcd = '{:02x}{:02x}'.format(*packet[offset:offset+2])

    return datetime.datetime.strptime(bcd, '%H%M').time()

def unpack_bool(packet, offset):
    return packet[offset] != 0x00

