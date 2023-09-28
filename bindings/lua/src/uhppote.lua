local uhppote = {}
local encode = require("src/encode")
local udp = require("src/udp")

function uhppote.set_debug(debug)
    udp.set_debug(debug)
end

function uhppote.get_all_controllers()
    request = encode.get_controller_request(0)
    replies = udp.broadcast(request)

    if not replies then
        error("no response")
    else
        print(">>>> replies:" .. #replies)
    end

    list = {}
    -- for _, reply := range replies {
    --     if response, err := getControllerResponse(reply); err != nil {
    --         return nil, err
    --     } else if response != nil {
    --         list = append(list, response)
    --     }
    -- }

    return list
end

return uhppote