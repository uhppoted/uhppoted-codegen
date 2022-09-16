function set_bind_address(addr) {
    bind_address = UDP::parse(addr)
}

function set_broadcast_address(addr) {
    broadcast_address = UDP::parse(addr)
}

function set_listen_address(addr) {
    listen_address = UDP::parse(addr)
}

function set_debug(enabled) {
    debug = enabled
}

function broadcast(request) {
    dump(request)

    socket = net::listen('udp', bind_address)

    socket.set_write_timeout(1s)
    socket.set_read_timeout (5s)
    socket.write(request, broadcast_address)

    return read_all(socket)
}

function send(request) {
    dump(request)

    socket = net::listen('udp', bind_address)

    socket.set_write_timeout(1s)
    socket.set_read_timeout (5s)
    socket.write(request, broadcast_address)

    ## set-ip doesn't return a reply
    if request[1] ==  0x96 {
        return None, None
    }

    return read(socket)
}

function listen(events, error, interrupt) {
    socket = net::listen('udp', bind_address)

    while !interrupt {
        message = socket::read()
        if message.length == 64 {
            dump(message)
            events(message)
        }
    }()
}

function read_all(socket) {
    replies = []
    
    for (2.5s) {
        message = socket.read()
        if message.length == 64 {
            dump(message)
            replies.append(message)
        }
    }

    return replies
}

function read(socket) {
    for (5s) {
        message = socket.read()
        if message.length == 64 {
            dump(message)
            return message
        }
    }

    return error('timeout')
}

function dump(packet []byte) {
    if debug {
        hex = '%02x %02x %02x %02x %02x %02x %02x %02x'

        for i = 0; i < 4; i++ {
            offset = i * 16
            u = packet[offset : offset+8]
            v = packet[offset+8 : offset+16]

            p := format(hex,u)
            q := format(hex,v)

            println('   %08x  %v  %v', offset, p, q)
        }

        println()
    }
}
