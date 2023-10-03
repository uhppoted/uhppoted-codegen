local uhppote = require("src/uhppote")

local CONTROLLER <const> = 405419896

function get_all_controllers()
    return uhppote.get_all_controllers()
end

function get_controller()
    local controller = CONTROLLER

    return uhppote.get_controller(controller)
end

local commands = {
   ["get-all-controllers"] = get_all_controllers,
   ["get-controller"] = get_controller,
}

function commands.exec(cmd)
    local response = cmd()

    if cmd == get_all_controllers then
        for k, v in ipairs(response) do
            pprint(v)
        end
    else
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
