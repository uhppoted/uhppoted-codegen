local uhppote = {}
local encode = require("src/encode")

function uhppote.get_all_controllers()
    request = encode.get_controller_request(0)

    print(request)
    -- if err != nil {
    --     return nil, err
    -- }

    -- replies, err := broadcast(request)
    -- if err != nil {
    --     return nil, err
    -- }

    -- list := []*GetControllerResponse{}
    -- for _, reply := range replies {
    --     if response, err := getControllerResponse(reply); err != nil {
    --         return nil, err
    --     } else if response != nil {
    --         list = append(list, response)
    --     }
    -- }

    -- return list, nil

    return "*** NOT IMPLEMENTED YET ***"
end

return uhppote