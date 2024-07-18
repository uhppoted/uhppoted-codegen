package uhppote

import (
    "net/netip"
    "os"
)

type Controller struct {
    controller uint32
    address    string
    transport  string
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
    controller := resolve(deviceId)

    {{if .response -}}
    request,err := {{CamelCase .request.name}}({{template "params" .args}})
    if err != nil {
        return nil,err
    }

    if reply,err := send(controller, request); err != nil {
        return nil,err
    } else if response,err := {{camelCase .response.name}}(reply); err != nil {
        return nil, err
    } else if response != nil {
        return response, nil
    }

    return nil, nil
    {{- else }}
    request, err := {{CamelCase .request.name}}({{template "params" .args}})
    if err != nil {
        return err
    } 

    if _, err = send(controller, request); err != nil {
        return err
    }
    
    return nil
    {{- end}}
}{{end}}

func resolve(controller uint32) Controller {
    address := ""
    transport := "udp"

    // if controller == 405419896 {
    //     address = "192.168.1.100:60000"
    //     transport = "tcp"
    // }

    return Controller {
        controller: controller,
        address: address,
        transport: transport,
    }    
}
