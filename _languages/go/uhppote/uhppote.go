package uhppote

import(
    "fmt"
    "net"
    "net/netip"    
    "reflect"
    "time"
)

const ANY = "0.0.0.0:0"
const BROADCAST = "192.168.1.255:60000"
const WRITE_TIMEOUT = 1000 * time.Millisecond
var NEVER = time.Time{}

{{range .Functions}}{{template "function" .}}
{{end}}

{{define "function"}}
func {{CamelCase .Name}}({{template "args" .Args}}) (any,error) {
    fmt.Printf(">> {{.Name}}\n")

    request,err := {{CamelCase .Request.Name}}({{template "params" .Args}})
    if err != nil {
        return nil,err
    }

    fmt.Printf(">> %v\n", request)

    replies,err := send(request, {{.Wait}} * time.Millisecond)
    if err != nil {
        return nil,err
    }

    fmt.Printf(">> %v\n", replies)

    list := []any{}
    for _,reply := range replies {
        if response,err := decode(reply); err != nil {
            return nil, err
        } else if response != nil && !reflect.ValueOf(response).IsNil(){
            list = append(list, response)
        }
    }

    return list,nil
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

{{define "args"}}{{range .}}{{camelCase .Name}} {{template "type" .Type}}{{end}}{{end}}

{{define "params"}}{{range .}}{{camelCase .Name}}{{end}}{{end}}

{{define "type"}}{{if eq . "uint32"}}uint32{{else}}any{{end}}{{end}}
