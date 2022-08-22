package uhppote

import (
    "net/netip"
)

func GetAllControllers() ([]*GetControllerResponse, error) {
    request, err := GetControllerRequest(0)
    if err != nil {
        return nil, err
    }

    replies, err := send(request, readAll)
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

{{range .model.functions}}{{template "function" .}}
{{end}}

{{define "function"}}
func {{CamelCase .name}}({{template "args" .args}}) {{if .response}}(*{{CamelCase .response.name}},error){{else}}error{{end}} {
    {{if .response}}request,err := {{CamelCase .request.name}}({{template "params" .args}})
    if err != nil {
        return nil,err
    }

    replies,err := send(request, read)
    if err != nil {
        return nil,err
    }

    for _,reply := range replies {
        if response,err := {{camelCase .response.name}}(reply); err != nil {
            return nil, err
        } else if response != nil {
            return response, nil
        }
    }

    return nil, nil{{else}}
    if request, err := {{CamelCase .request.name}}({{template "params" .args}}); err != nil {
        return err
    } else if _, err = send(request, readNone); err != nil {
        return err
    }
    
    return nil{{end}}
}{{end}}
