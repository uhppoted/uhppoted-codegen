local uhppote = {}
local encode = require("src/encode")
local decode = require("src/decode")
local udp = require("src/udp")

function uhppote.set_debug(debug)
    udp.set_debug(debug)
end

function uhppote.get_all_controllers()
    request = encode.get_controller_request(0)
    replies = udp.broadcast(request)

    if not replies then
        error("no response")
    end

    list = {}
    for k, reply in ipairs(replies) do
        response = decode.get_controller_response(reply)
        for k,v in pairs(response) do
            print(k, v)
        end
        print()
    end
    --     if response, err := getControllerResponse(reply); err != nil {
    --         return nil, err
    --     } else if response != nil {
    --         list = append(list, response)
    --     }

    return list
end

return uhppote