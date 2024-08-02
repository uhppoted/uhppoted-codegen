package uhppote

import (
    "net/netip"
    "os"
    "fmt"
)

type Controller struct {
    Controller uint32
    Address    string
    Transport  string
}

func GetAllControllers() ([]*GetControllerResponse, error) {
    request, err := GetControllerRequest(0)
    if err != nil {
        return nil, err
    }

    replies, err := broadcast(request)
    if err != nil {
        return nil, err
    }

    list := []*GetControllerResponse{}
    for _, reply := range replies {
        if response, err := getControllerResponse(reply); err != nil {
            return nil, err
        } else if response != nil {
            list = append(list, response)
        }
    }

    return list, nil
}

func Listen(events chan Event, errors chan error, interrupt chan os.Signal) error {
    ch := make(chan []uint8)

    go listen(ch)

loop:
    for {
        select {
        case msg := <-ch:
            if evt, err := event(msg); err != nil {
                errors <- err
            } else if evt != nil {
                events <- *evt
            }

        case <-interrupt:
            break loop
        }
    }

    return nil
}
{{- range .model.functions }}
{{ template "function" . -}}
{{end}}

{{define "function"}}
func {{CamelCase .name}}({{template "args" .args}}) {{if .response}}(*{{CamelCase .response.name}},error){{else}}error{{end}} {
    c := resolve(controller)

    {{if .response -}}
    request,err := {{CamelCase .request.name}}(c.Controller, {{template "params" slice .args 1}})
    if err != nil {
        return nil,err
    }

    if reply,err := send(c, request); err != nil {
        return nil,err
    } else if response,err := {{camelCase .response.name}}(reply); err != nil {
        return nil, err
    } else if response != nil {
        return response, nil
    }

    return nil, nil
    {{- else }}
    request, err := {{CamelCase .request.name}}(c.Controller, {{template "params" slice .args 1}})
    if err != nil {
        return err
    } 

    if _, err = send(c, request); err != nil {
        return err
    }
    
    return nil
    {{- end}}
}{{end}}

func resolve(controller any) Controller {
    switch v := controller.(type) {
        case uint32:
            address := ""
            transport := "udp"

            // if v == 405419896 {
            //     address = "192.168.1.100:60000"
            //     transport = "tcp"
            // }

            return Controller {
                Controller: v,
                Address: address,
                Transport: transport,
            }    

        case Controller:
            return v
    }

    panic(fmt.Sprintf("unknown controller type (%T)", controller))
}
