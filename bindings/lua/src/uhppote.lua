local uhppote = {}
local encode = require("src/encode")
local decode = require("src/decode")
local udp = require("src/udp")

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

return uhppote