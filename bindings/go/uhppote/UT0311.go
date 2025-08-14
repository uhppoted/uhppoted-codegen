package uhppote

import (
    "fmt"
    "net"
    "net/netip"
    "time"
    "syscall"
)

const CONNECT_TIMEOUT = 1000 * time.Millisecond
const READ_TIMEOUT = 5000 * time.Millisecond
const WRITE_TIMEOUT = 1000 * time.Millisecond

var bindAddr netip.AddrPort = netip.MustParseAddrPort("0.0.0.0:0")
var broadcastAddr *net.UDPAddr = resolveAddr("255.255.255.255:60000")
var listenAddr *net.UDPAddr = resolveAddr("0.0.0.0:60001")
var debug bool = false

func SetBindAddr(addr string) {
    address := netip.MustParseAddr(addr)
    bindAddr = netip.AddrPortFrom(address, 0)
}

func SetBroadcastAddr(addr string) {
    broadcastAddr = resolveAddr(addr)
}

func SetListenAddr(addr string) {
    listenAddr = resolveAddr(addr)
}

func SetDebug(enabled bool) {
    debug = enabled
}

func broadcast(request []byte) ([][]byte, error) {
    dump(request)

    bind := net.UDPAddrFromAddrPort(bindAddr)
    if bind == nil {
        bind = &net.UDPAddr{
            IP:   net.IPv4(0, 0, 0, 0),
            Port: 0,
            Zone: "",
        }
    }

    socket, err := net.ListenUDP("udp", bind)
    if err != nil {
        return nil, err
    }

    defer socket.Close()

    if err := socket.SetWriteDeadline(time.Now().Add(WRITE_TIMEOUT)); err != nil {
        return nil, err
    }

    if err := socket.SetReadDeadline(time.Now().Add(READ_TIMEOUT)); err != nil {
        return nil, fmt.Errorf("Failed to set UDP read timeout [%v]", err)
    }

    if _, err := socket.WriteToUDP(request, broadcastAddr); err != nil {
        return nil, err
    }

    return readAll(socket)
}

func send(controller Controller, request []byte) ([]byte, error) {
    if controller.Address != "" && controller.Transport == "tcp" {
        if addr, err := netip.ParseAddrPort(controller.Address); err != nil {
            return nil, err
        } else {
            return tcpSendTo(request, net.TCPAddrFromAddrPort(addr))
        }
    } 

    if controller.Address != "" && controller.Transport == "udp" {
        if addr, err := netip.ParseAddrPort(controller.Address); err != nil {
            return nil, err
        } else {
            return udpSendTo(request, net.UDPAddrFromAddrPort(addr))
        }
    } 
      
    return udpBroadcastTo(request)
}

func udpBroadcastTo(request []byte) ([]byte, error) {
    dump(request)

    bind := net.UDPAddrFromAddrPort(bindAddr)
    if bind == nil {
        bind = &net.UDPAddr{
            IP:   net.IPv4(0, 0, 0, 0),
            Port: 0,
            Zone: "",
        }
    }

    if connection, err := net.ListenUDP("udp", bind); err != nil {
        return nil, fmt.Errorf("error creating UDP socket (%v)", err)
    } else if connection == nil {
        return nil, fmt.Errorf("open() created invalid UDP socket (%v)", connection)
    } else {
        defer connection.Close()

        deadline := time.Now().Add(READ_TIMEOUT)
        if err := connection.SetDeadline(deadline); err != nil {
            return nil, err
        }

        if _, err := connection.WriteToUDP(request, broadcastAddr); err != nil {
            return nil, fmt.Errorf("failed to write to UDP socket [%v]", err)
        }

        // NTS: set-ip doesn't return a reply so fake it
        if request[1] == 0x96 {
            reply := make([]byte, 64)

            copy(reply, request[:8])
            reply[8] = 0x01

            return reply, nil
        }

        for {
            reply := make([]byte, 2048)

            if N, _, err := connection.ReadFromUDP(reply); err != nil {
                return nil, err
            } else {
                dump(reply[:N])

                return reply[:N], err
            }
        }
    }
}

func udpSendTo(request []byte, addr *net.UDPAddr) ([]byte, error) {
    dump(request)

    address := fmt.Sprintf("%v", addr)
    bind := net.UDPAddrFromAddrPort(bindAddr)

    if bind == nil {
        bind = &net.UDPAddr{
            IP:   net.IPv4(0, 0, 0, 0),
            Port: 0,
            Zone: "",
        }
    }

    dialer := net.Dialer{
        Deadline:  time.Now().Add(CONNECT_TIMEOUT),
        LocalAddr: bind,
        Control: func(network, address string, connection syscall.RawConn) (err error) {
            var operr error

            f := func(fd uintptr) {
                operr = setSocketOptions(fd)
            }

            if err := connection.Control(f); err != nil {
                return err
            } else {
                return operr
            }
        },
    }

    if connection, err := dialer.Dial("udp4", address); err != nil {
        return nil, err
    } else if connection == nil {
        return nil, fmt.Errorf("invalid UDP socket (%v)", connection)
    } else {
        defer connection.Close()

        deadline := time.Now().Add(READ_TIMEOUT)
        buffer := make([]byte, 1024)

        if err := connection.SetDeadline(deadline); err != nil {
            return nil, err
        }

        if _, err := connection.Write(request); err != nil {
            return nil, fmt.Errorf("failed to write to UDP socket [%v]", err)
        }

        // NTS: set-ip doesn't return a reply
        if request[1] == 0x96 {
            return nil, nil
        }

        if N, err := connection.Read(buffer); err != nil {
            return nil, err
        } else {
            dump(buffer[:N])

            return buffer[0:N], nil
        }
    }
}

func tcpSendTo(request []byte, addr *net.TCPAddr) ([]byte, error) {
    dump(request)

    address := fmt.Sprintf("%v", addr)
    bind := net.TCPAddrFromAddrPort(bindAddr)

    if bind == nil {
        bind = &net.TCPAddr{
            IP:   net.IPv4(0, 0, 0, 0),
            Port: 0,
            Zone: "",
        }
    }

    dialer := net.Dialer{
        Deadline:  time.Now().Add(CONNECT_TIMEOUT),
        LocalAddr: bind,
        Control: func(network, address string, connection syscall.RawConn) (err error) {
            var operr error

            f := func(fd uintptr) {
                operr = setSocketOptions(fd)
            }

            if err := connection.Control(f); err != nil {
                return err
            } else {
                return operr
            }
        },
    }

    if connection, err := dialer.Dial("tcp4", address); err != nil {
        return nil, err
    } else if connection == nil {
        return nil, fmt.Errorf("invalid TCP socket (%v)", connection)
    } else {
        defer connection.Close()

        deadline := time.Now().Add(READ_TIMEOUT)
        buffer := make([]byte, 1024)

        if err := connection.SetDeadline(deadline); err != nil {
            return nil, err
        }

        if _, err := connection.Write(request); err != nil {
            return nil, fmt.Errorf("failed to write to UDP socket [%v]", err)
        }

        // NTS: set-ip doesn't return a reply
        if request[1] == 0x96 {
            return nil, nil
        }

        if N, err := connection.Read(buffer); err != nil {
            return nil, err
        } else {
            dump(buffer[:N])

            return buffer[0:N], nil
        }
    }
}

func listen(ch chan []byte) error {
    socket, err := net.ListenUDP("udp", listenAddr)
    if err != nil {
        return err
    }

    defer socket.Close()

    events := make(chan []byte)
    e := make(chan error)

    go func() {
        buffer := make([]byte, 1024)

        for {
            if N, _, err := socket.ReadFromUDP(buffer); err != nil {
                e <- err
            } else if N == 64 {
                m := make([]byte, 64)
                copy(m, buffer[0:N])
                events <- m
            }
        }
    }()

    for {
        select {
        case m := <-events:
            dump(m)
            ch <- m

        case err := <-e:
            return err
        }
    }

    return nil
}

func readAll(socket *net.UDPConn) ([][]byte, error) {
    reply := make(chan []byte)
    e := make(chan error)

    go func() {
        buffer := make([]byte, 1024)

        for {
            if N, _, err := socket.ReadFromUDP(buffer); err != nil {
                e <- err
            } else if N == 64 {
                m := make([]byte, 64)
                copy(m, buffer[0:N])
                reply <- m
            }
        }
    }()

    replies := [][]byte{}
    wait := time.After(2500 * time.Millisecond)

    for {
        select {
        case m := <-reply:
            dump(m)
            replies = append(replies, m)

        case <-wait:
            return replies, nil

        case err := <-e:
            return nil, err
        }
    }
}

func resolveAddr(address string) *net.UDPAddr {
    addr := netip.MustParseAddrPort(address)

    return net.UDPAddrFromAddrPort(addr)
}

func dump(packet []byte) {
    if debug {
        hex := "%02x %02x %02x %02x %02x %02x %02x %02x"

        for i := 0; i < 4; i++ {
            offset := i * 16
            u := packet[offset : offset+8]
            v := packet[offset+8 : offset+16]

            p := fmt.Sprintf(hex, u[0], u[1], u[2], u[3], u[4], u[5], u[6], u[7])
            q := fmt.Sprintf(hex, v[0], v[1], v[2], v[3], v[4], v[5], v[6], v[7])

            fmt.Printf("   %08x  %v  %v\n", offset, p, q)
        }

        fmt.Println()
    }
}
