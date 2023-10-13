local uhppote = {}
local encode = require("src/encode")
local decode = require("src/decode")
local udp = require("src/udp")

function uhppote.set_bind_address(address)
    udp.set_bind_address(address)
end

function uhppote.set_broadcast_address(address)
    udp.set_broadcast_address(address)
end

function uhppote.set_listen_address(address)
    udp.set_listen_address(address)
end

function uhppote.set_debug(debug)
    udp.set_debug(debug)
end

function uhppote.get_all_controllers()
    local request = encode.get_controller_request(0)
    local replies = udp.broadcast(request)

    if not replies then
        error("no response")
    end

    local list = {}
    for k, reply in ipairs(replies) do
        local response = decode.get_controller_response(reply)

        table.insert(list, response)
    end

    return list
end

function uhppote.listen(handler, onerror)
    local f = function(packet) 
                 local ok, event = xpcall(function() return decode.event(packet) end, onerror)
                 if ok then
                    handler(event)
                 end
              end

    return udp.listen(f)
end

{{range .model.functions}}
{{- template "function" . -}}
{{end}}

{{define "function"}}
function uhppote.{{snakeCase .name}}({{template "args" .args}})
    local request = encode.{{snakeCase .request.name}}({{template "params" .args}})
    local reply = udp.send(request)
    {{if .response}}
    if not reply then
        error("no response")
    end
    
    return decode.{{snakeCase .response.name}}(reply)
    {{- else}}
    return nil
    {{- end}}
end
{{end}}

return uhppote