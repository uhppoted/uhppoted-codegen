import socket
import struct

READ_TIMEOUT = struct.pack('ll', 5, 0)
WRITE_TIMEOUT = struct.pack('ll', 1, 0)


class UDP:

    def __init__(self, bind='0.0.0.0', broadcast='255.255.255.255:60000', debug=False):
        self._bind = ('0.0.0.0', 0)
        self._broadcast = ('192.168.1.100', 60000)
        self._debug = debug

    def send(self, request, fn):
        self.dump(request)

        # sock = socket.socket(socket.AF_INET, socket.SOCK_DGRAM | socket.SOCK_NONBLOCK)
        sock = socket.socket(socket.AF_INET, socket.SOCK_DGRAM)

        try:
            sock.bind(self._bind)
            sock.setsockopt(socket.SOL_SOCKET, socket.SO_SNDTIMEO, WRITE_TIMEOUT)
            sock.setsockopt(socket.SOL_SOCKET, socket.SO_RCVTIMEO, READ_TIMEOUT)
            # socket, err := net.ListenUDP("udp", bindAddr)

            sock.sendto(request, self._broadcast)

            return fn(sock, self._debug)
        finally:
            sock.close()

    def dump(self, packet):
        if self._debug:
            dump(packet)


def read_all(sock, debug):
    replies = []
    reply = sock.recv(1024)
    if len(reply) == 64:
        replies.append(reply)
        if debug:
            dump(reply)

    return replies


def dump(packet):
    for i in range(0, 4):
        offset = i * 16
        u = packet[offset:offset + 8]
        v = packet[offset + 8:offset + 16]

        p = f'{u[0]:02x} {u[1]:02x} {u[2]:02x} {u[3]:02x} {u[4]:02x} {u[5]:02x} {u[6]:02x} {u[7]:02x}'
        q = f'{v[0]:02x} {v[1]:02x} {v[2]:02x} {v[3]:02x} {v[4]:02x} {v[5]:02x} {v[6]:02x} {v[7]:02x}'

        print(f'   {offset:08x}  {p}  {q}')

    print()
