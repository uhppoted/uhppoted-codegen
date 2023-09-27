local uhppote = {}
local encode = require("src/encode")
local udp = require("src/udp")

function uhppote.set_debug(debug)
    udp.set_debug(debug)
end

function uhppote.get_all_controllers()
    request = encode.get_controller_request(0)
    replies = udp.broadcast(request)

    -- list := []*GetControllerResponse{}
    -- for _, reply := range replies {
    --     if response, err := getControllerResponse(reply); err != nil {
    --         return nil, err
    --     } else if response != nil {
    --         list = append(list, response)
    --     }
    -- }

    -- return list, nil

    error("*** NOT IMPLEMENTED ***")
end

return uhppote