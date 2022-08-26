package uhppote

import (
    "fmt"
    "net"
    "net/netip"
    "time"
)

const READ_TIMEOUT = 5000 * time.Millisecond
const WRITE_TIMEOUT = 1000 * time.Millisecond

var bindAddr *net.UDPAddr = resolve("0.0.0.0:0")
var destAddr *net.UDPAddr = resolve("255.255.255.255:60000")
var debug bool = false

func SetBindAddr(addr string) {
    address := netip.MustParseAddr(addr)
    addrPort := netip.AddrPortFrom(address, 0)
    bindAddr = net.UDPAddrFromAddrPort(addrPort)
}

func SetDestAddr(addr string) {
    destAddr = resolve(addr)
}

func SetDebug(enabled bool) {
    debug = enabled
}

func send(request []byte, f func(*net.UDPConn) ([][]byte, error)) ([][]byte, error) {
    dump(request)

    socket, err := net.ListenUDP("udp", bindAddr)
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

    if _, err := socket.WriteToUDP(request, destAddr); err != nil {
        return nil, err
    }

    return f(socket)
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

func readNone(socket *net.UDPConn) ([][]byte, error) {
    return [][]byte{}, nil
}

func read(socket *net.UDPConn) ([][]byte, error) {
    reply := make(chan []byte)
    e := make(chan error)

    go func() {
        buffer := make([]byte, 1024)

        for {
            if N, _, err := socket.ReadFromUDP(buffer); err != nil {
                e <- err
            } else if N == 64 {
                reply <- buffer[0:N]
            }
        }
    }()

    timeout := time.After(5000 * time.Millisecond)

    for {
        select {
        case m := <-reply:
            dump(m)
            return [][]byte{m}, nil

        case <-timeout:
            return nil, fmt.Errorf("timeout")

        case err := <-e:
            return nil, err
        }
    }
}

func resolve(address string) *net.UDPAddr {
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
