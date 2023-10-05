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

function uhppote.get_controller(device_id)
    local request = encode.get_controller_request(device_id)
    local reply = udp.send(request)

    if not reply then
        error("no response")
    end

    return decode.get_controller_response(reply)
end

function uhppote.set_ip(device_id, address, netmask, gateway)
    local request = encode.set_ip_request(device_id,address,netmask,gateway)
    local reply = udp.send(request)

    return nil
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

-- func Listen(events chan Event, errors chan error, interrupt chan os.Signal) error {
--         ... 
--         case <-interrupt:
--             break loop
--         }
--     }


return uhppote