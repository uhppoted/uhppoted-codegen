import socket
import struct
import re
import time
import ipaddress

READ_TIMEOUT = struct.pack('ll', 5, 0)
WRITE_TIMEOUT = struct.pack('ll', 1, 0)
NO_TIMEOUT = struct.pack('ll', 0, 0)


class Net:

    def __init__(self, bind='0.0.0.0', broadcast='255.255.255.255:60000', listen="0.0.0.0:60001", debug=False):
        self._bind = (bind, 0)
        self._broadcast = resolve(broadcast)
        self._listen = resolve(listen)
        self._debug = debug

    def broadcast(self, request):
        self.dump(request)

        sock = socket.socket(socket.AF_INET, socket.SOCK_DGRAM, 0)

        try:
            sock.bind(self._bind)
            sock.setsockopt(socket.SOL_SOCKET, socket.SO_BROADCAST, 1)
            sock.setsockopt(socket.SOL_SOCKET, socket.SO_SNDTIMEO, WRITE_TIMEOUT)
            sock.setsockopt(socket.SOL_SOCKET, socket.SO_RCVTIMEO, READ_TIMEOUT)

            sock.sendto(request, self._broadcast)

            return read_all(sock, self._debug)
        finally:
            sock.close()

    def send(self, controller, request):
        self.dump(request)

        if controller.transport == 'tcp' and not controller.address is None and controller.address != '':
            address = resolve(controller.address)

            return self.tcp_sendto(request, address)            
        elif not controller.address is None and controller.address != '':
            address = resolve(controller.address)

            return self.udp_sendto(request, address)
        else:
            return self.udp_broadcast_to(request)

    def udp_broadcast_to(self, request):
        with socket.socket(socket.AF_INET, socket.SOCK_DGRAM, 0) as sock:
            sock.bind(self._bind)
            sock.setsockopt(socket.SOL_SOCKET, socket.SO_BROADCAST, 1)
            sock.setsockopt(socket.SOL_SOCKET, socket.SO_SNDTIMEO, WRITE_TIMEOUT)
            sock.setsockopt(socket.SOL_SOCKET, socket.SO_RCVTIMEO, READ_TIMEOUT)

            sock.sendto(request, self._broadcast)

            if request[1] == 0x96:
                return None

            return read(sock, self._debug)

    def udp_sendto(self, request, address):
        with socket.socket(socket.AF_INET, socket.SOCK_DGRAM, 0) as sock:
            sock.bind(self._bind)
            sock.setsockopt(socket.SOL_SOCKET, socket.SO_SNDTIMEO, WRITE_TIMEOUT)
            sock.setsockopt(socket.SOL_SOCKET, socket.SO_RCVTIMEO, READ_TIMEOUT)

            sock.sendto(request, address)

            if request[1] == 0x96:
                return None

            return read(sock, self._debug)

    def tcp_sendto(self, request, address):
        with socket.socket(socket.AF_INET, socket.SOCK_STREAM) as sock:
            sock.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, 1)
            sock.setsockopt(socket.SOL_SOCKET, socket.SO_SNDTIMEO, WRITE_TIMEOUT)
            sock.setsockopt(socket.SOL_SOCKET, socket.SO_RCVTIMEO, READ_TIMEOUT)

            if not is_INADDR_ANY(self._bind):
                sock.bind(self._bind)

            sock.connect(address)
            sock.sendall(request)

            if request[1] == 0x96:
                return None
            else:
                return read(sock, self._debug)

    def listen(self, events):
        sock = socket.socket(socket.AF_INET, socket.SOCK_DGRAM, 0)

        try:
            sock.bind(self._listen)
            sock.setsockopt(socket.SOL_SOCKET, socket.SO_RCVTIMEO, NO_TIMEOUT)

            while True:
                message = sock.recv(1024)
                if len(message) == 64:
                    if self._debug:
                        dump(message)
                    events(message)
        finally:
            sock.close()

    def dump(self, packet):
        if self._debug:
            dump(packet)


# TODO convert to asyncio
def read(sock, debug):
    sock.settimeout(2.5)

    while True:
        reply = sock.recv(1024)
        if len(reply) == 64:
            if debug:
                dump(reply)
            return reply

    return None


# TODO convert to asyncio
def read_all(sock, debug):
    sock.settimeout(2.5)

    replies = []
    while True:
        try:
            reply = sock.recv(1024)
            if len(reply) == 64:
                replies.append(reply)
                if debug:
                    dump(reply)
        except socket.timeout:
            break

    return replies


def resolve(addr):
    match = re.match(r'(.*?):([0-9]+)', addr)
    if match:
        return (match.group(1), int(match.group(2)))
    else:
        address = ipaddress.IPv4Address(addr)
        return (str(address), 60000)

def is_INADDR_ANY(addr):
    if addr == None:
        return True

    if f'{addr}' == '':
        return True

    if addr == (('0.0.0.0', 0)):
        return True

    return False

def dump(packet):
    for i in range(0, 4):
        offset = i * 16
        u = packet[offset:offset + 8]
        v = packet[offset + 8:offset + 16]

        p = f'{u[0]:02x} {u[1]:02x} {u[2]:02x} {u[3]:02x} {u[4]:02x} {u[5]:02x} {u[6]:02x} {u[7]:02x}'
        q = f'{v[0]:02x} {v[1]:02x} {v[2]:02x} {v[3]:02x} {v[4]:02x} {v[5]:02x} {v[6]:02x} {v[7]:02x}'

        print(f'   {offset:08x}  {p}  {q}')

    print()
