import struct


def get_controller_request(device_id):
    request = bytearray(64)

    request[0] = 0x17
    request[1] = 0x94

    pack_uint32(device_id, request, 4)

    return request


def pack_uint32(v, packet, offset):
    struct.pack_into('<I', packet, offset, v)
