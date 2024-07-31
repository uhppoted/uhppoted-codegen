local uhppote = {}
local encode = require("src/encode")
local decode = require("src/decode")
local ut0311 = require("src/ut0311")

function uhppote.set_bind_address(address)
    ut0311.set_bind_address(address)
end

function uhppote.set_broadcast_address(address)
    ut0311.set_broadcast_address(address)
end

function uhppote.set_listen_address(address)
    ut0311.set_listen_address(address)
end

function uhppote.set_debug(debug)
    ut0311.set_debug(debug)
end

function uhppote.get_all_controllers()
    local request = encode.get_controller_request(0)
    local replies = ut0311.broadcast(request)

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
        local ok, event = xpcall(function()
            return decode.event(packet)
        end, onerror)
        if ok then
            handler(event)
        end
    end

    ut0311.listen(f)
end

{{range .model.functions}}
{{- template "function" . -}}
{{end}}

{{define "function"}}
function uhppote.{{snakeCase .name}}({{template "args" .args}})
    local c = resolve(controller)
    local request = encode.{{snakeCase .request.name}}({{template "params" .args}})
    local reply = ut0311.send(c, request)
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

function resolve(controller)
    local c = {
        controller = controller,
        address = "",
        transport = "udp",

        fields = function(self)
            return {
                "controller",
                "address",
                "transport"
            }
        end,
    }

    return c
end

return uhppote