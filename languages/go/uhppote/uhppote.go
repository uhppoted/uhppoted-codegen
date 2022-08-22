package uhppote

import (
    "fmt"
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

{{range .model.Functions}}{{template "function" .}}
{{end}}

{{define "function"}}
func {{CamelCase .Name}}({{template "args" .Args}}) {{if .Response}}(*{{CamelCase .Response.Name}},error){{else}}error{{end}} {
    fmt.Printf(">> {{.Name}}\n")
    {{if .Response}}
    request,err := {{CamelCase .Request.Name}}({{template "params" .Args}})
    if err != nil {
        return nil,err
    }

    replies,err := send(request, read)
    if err != nil {
        return nil,err
    }

    for _,reply := range replies {
        if response,err := {{camelCase .Response.Name}}(reply); err != nil {
            return nil, err
        } else if response != nil {
            return response, nil
        }
    }

    return nil, nil{{else}}
    if request, err := {{CamelCase .Request.Name}}({{template "params" .Args}}); err != nil {
        return err
    } else if _, err = send(request, readNone); err != nil {
        return err
    }
    
    return nil{{end}}
}{{end}}
