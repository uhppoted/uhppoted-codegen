import datetime
import struct

from ipaddress import IPv4Address
from dataclasses import dataclass


def get_controller_response(packet):
    if len(packet) != 64:
        raise ValueError(f'invalid reply packet length ({len(packet)})')

    if packet[1] != 0x94:
        raise ValueError(f'invalid reply function code ({packet[1]:02x})')

    return GetControllerResponse(
        unpack_uint32(packet, 4),
        unpack_IPv4(packet, 8),
        unpack_IPv4(packet, 12),
        unpack_IPv4(packet, 16),
        unpack_MAC(packet, 20),
        unpack_version(packet, 26),
        unpack_date(packet, 28),
    )


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


@dataclass
class GetControllerResponse:
    device_id: int
    address: IPv4Address
    subnet: IPv4Address
    gateway: IPv4Address
    MAC: str
    version: str
    date: datetime.date
