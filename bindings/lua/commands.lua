local uhppote = require("src/uhppote")

local CONTROLLER <const> = 405419896
local ADDRESS <const> = "192.168.1.100"
local NETMASK <const> = "255.255.255.0"
local GATEWAY <const> = "192.168.1.1"

function get_all_controllers()
    return uhppote.get_all_controllers()
end

function get_controller()
    local controller = CONTROLLER

    return uhppote.get_controller(controller)
end

function set_ip()
    local controller = CONTROLLER
    local address = ADDRESS
    local netmask = NETMASK
    local gateway = GATEWAY

    return uhppote.set_ip(controller,address,netmask,gateway)
end

local commands = {
   commands = {     
       ["get-all-controllers"] = get_all_controllers,
       ["get-controller"] = get_controller,
       ["set-ip"] = set_ip,
   },
}

function commands.exec(cmd)
    local response = cmd()

    if cmd == get_all_controllers then
        for k, v in ipairs(response) do
            pprint(v)
        end
    elseif response then
        pprint(response)
    end
end

function pprint(v)
    local fields = v.fields()
    local w = 0

    for k,f in ipairs(fields) do
        if #f > w then
            w = #f
        end
    end

    local fmt = string.format("   %%-%ds  ",w)

    for k,f in ipairs(fields) do
        print(string.format(fmt,f) .. v[f])
    end

    print()
end

return commands
