package uhppote

import(
    "encoding/hex"
    "fmt"
    "net"
    "net/netip"    
    "regexp"
    "time"
)

const ANY = "0.0.0.0:0"
const BROADCAST = "192.168.1.255:60000"
const WRITE_TIMEOUT = 1000 * time.Millisecond
var NEVER = time.Time{}

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

{{range .Functions}}{{template "function" .}}
{{end}}

{{define "function"}}
func {{CamelCase .Name}}({{template "args" .Args}}) (*{{CamelCase .Response.Name}},error) {
    fmt.Printf(">> {{.Name}}\n")

    request,err := {{CamelCase .Request.Name}}({{template "params" .Args}})
    if err != nil {
        return nil,err
    }

    fmt.Printf("%v\n", dump(request, "   "))

    replies,err := send(request, {{.Wait}} * time.Millisecond)
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
    any := netip.MustParseAddrPort(ANY)
    broadcast := netip.MustParseAddrPort(BROADCAST)
    
    bind := net.UDPAddrFromAddrPort(any)
    dest := net.UDPAddrFromAddrPort(broadcast)

    socket, err := net.ListenUDP("udp", bind)
    if err != nil {
        return nil, err
    }

    defer socket.Close()

    if err := socket.SetWriteDeadline(time.Now().Add(WRITE_TIMEOUT)); err != nil {
        return nil, err
    }

    if err := socket.SetReadDeadline(NEVER); err != nil {
        return nil, fmt.Errorf("Failed to set UDP read timeout [%v]", err)
    }

    if _, err := socket.WriteToUDP(request, dest); err != nil {
        return nil, err
    }

    reply := make(chan []byte)
    e := make(chan error)
    waited := time.After(wait)
    timeout := time.After((wait + 5000) * time.Millisecond)

    replies := [][]byte{}

    go func() {
        buffer := make([]byte, 2048)

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

        case <-timeout:
            return nil, fmt.Errorf("timeout")

        case err := <-e:
            return nil, err
        }
    }
}

func dump(m []byte, prefix string) string {
    p := regexp.MustCompile(`\s*\|.*?\|`).ReplaceAllString(hex.Dump(m), "")
    q := regexp.MustCompile("(?m)^(.*)").ReplaceAllString(p, prefix+"$1")

    return fmt.Sprintf("%s", q)
}

{{define "args"}}{{range .}}{{camelCase .Name}} {{template "type" .Type}}{{end}}{{end}}

{{define "params"}}{{range .}}{{camelCase .Name}}{{end}}{{end}}

{{define "type"}}{{if eq . "uint32"}}uint32{{else}}any{{end}}{{end}}
