local structs = {}

{{- range .model.responses}}
{{- template "response" . -}}
{{end}}

{{- define "response"}}
function structs.{{snakeCase .name}}({{- template "construct" .fields -}})
    local response = {
        {{range .fields}}{{snakeCase .name}} = {{snakeCase .name}},
        {{end}}
        fields = function(self) 
                     return { {{- range .fields}}
                         "{{snakeCase .name}}",{{end}}
                     }
                 end,
    }

    return response
end
{{end}}

{{with .model.event -}}
function structs.{{snakeCase .name}}({{- template "construct" .fields -}})
    local response = {
        {{range .fields}}{{snakeCase .name}} = {{snakeCase .name}},
        {{end}}
        fields = function(self) 
                     return { {{- range .fields}}
                         "{{snakeCase .name}}",{{end}}
                     }
                 end,
    }

    return response
end
{{end}}

function structs.controller(controller, address, transport)
    local t = {
        controller = controller,
        address = address,
        transport = transport,

        fields = function(self)
            return {
                "controller",
                "address",
                "transport"
            }
        end,
    }

    setmetatable(t, {
        __type = 'structs.controller'
    })

    return t
end


return structs