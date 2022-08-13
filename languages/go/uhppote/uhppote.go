package uhppote

import (
    "encoding/hex"
    "fmt"
    "net"
    "net/netip"
    "regexp"
    "time"
)

const READ_TIMEOUT = 5000 * time.Millisecond
const WRITE_TIMEOUT = 1000 * time.Millisecond

var bindAddr *net.UDPAddr
var destAddr *net.UDPAddr

func SetBindAddr(addr string) {
    bindAddr = resolve(addr)
}

func SetDestAddr(addr string) {
    destAddr = resolve(addr)
}

func GetAllControllers() ([]*GetControllerResponse, error) {
    request, err := GetControllerRequest(0)
    if err != nil {
        return nil, err
    }

    fmt.Printf("%v\n", dump(request, "   "))

    replies, err := send(request, 2500*time.Millisecond)
    if err != nil {
        return nil, err
    }

    list := []*GetControllerResponse{}
    for _, reply := range replies {
        fmt.Printf("%v\n", dump(reply, "   "))

        if response, err := getControllerResponse(reply); err != nil {
            return nil, err
        } else if response != nil {
            list = append(list, response)
        }
    }

    return list, nil
}

{{range .model.Functions}}{{template "function" .}}
{{end}}

{{define "function"}}
func {{CamelCase .Name}}({{template "args" .Args}}) (*{{CamelCase .Response.Name}},error) {
    fmt.Printf(">> {{.Name}}\n")

    request,err := {{CamelCase .Request.Name}}({{template "params" .Args}})
    if err != nil {
        return nil,err
    }

    fmt.Printf("%v\n", dump(request, "   "))

    replies,err := send(request, 0 * time.Millisecond)
    if err != nil {
        return nil,err
    }

    for _,reply := range replies {
        fmt.Printf("%v\n", dump(reply, "   "))

        if response,err := {{camelCase .Response.Name}}(reply); err != nil {
            return nil, err
        } else if response != nil {
            return response, nil
        }
    }

    return nil, nil
}{{end}}

func send(request []byte, wait time.Duration) ([][]byte, error) {
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

    reply := make(chan []byte)
    e := make(chan error)
    waited := time.After(wait)

    replies := [][]byte{}

    go func() {
        buffer := make([]byte, 1024)

        for {
            if N, _, err := socket.ReadFromUDP(buffer); err != nil {
                e <- err
            } else {
                reply <- buffer[0:N]
            }            
        }
    }()

    for {
        select {
        case m := <-reply:
            replies = append(replies, m)
            if wait == 0 {
                return replies, nil
            }

        case <-waited:
            if wait > 0 {
                return replies, nil                
            }

        case err := <-e:
            return nil, err
        }
    }
}

func resolve(address string) *net.UDPAddr {
    addr := netip.MustParseAddrPort(address)
    
    return net.UDPAddrFromAddrPort(addr)
}

func dump(m []byte, prefix string) string {
    p := regexp.MustCompile(`\s*\|.*?\|`).ReplaceAllString(hex.Dump(m), "")
    q := regexp.MustCompile("(?m)^(.*)").ReplaceAllString(p, prefix+"$1")

    return fmt.Sprintf("%s", q)
}
