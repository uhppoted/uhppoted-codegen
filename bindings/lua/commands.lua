local uhppote = require("src/uhppote")
local os = require("os")

local CONTROLLER <const> = 405419896
local ADDRESS <const> = "192.168.1.100"
local NETMASK <const> = "255.255.255.0"
local GATEWAY <const> = "192.168.1.1"
local SIGINT = 2

function get_all_controllers(args)
    return uhppote.get_all_controllers()
end

function get_controller(args)
    local controller = parse(args,"controller",CONTROLLER)

    return uhppote.get_controller(controller)
end

function set_ip(args)
    local controller = parse(args,"controller",CONTROLLER)
    local address = parse(args,"address",ADDRESS)
    local netmask = parse(args,"netmask",NETMASK)
    local gateway = parse(args,"gateway",GATEWAY)

    return uhppote.set_ip(controller,address,netmask,gateway)
end

function get_status(args)
    local controller = parse(args,"controller",CONTROLLER)

    return uhppote.get_status(controller)
end

function get_time(args)
    local controller = parse(args,"controller",CONTROLLER)

    return uhppote.get_time(controller)
end

function set_time(args)
    local controller = parse(args,"controller",CONTROLLER)
    local datetime = parse(args,"time",os.date("%Y-%m-%d %H:%M:%S"))

print(">>>>>>>>>>>>", datetime)
    return uhppote.set_time(controller,datetime)
end

function listen(args)
    local onerror = function(err)
                       print("   *** ERROR", err)
                   end

    local handler = function(event)
                       pprint(event)
                   end

    -- Ref. https://stackoverflow.com/questions/50506099/why-is-lua-not-respecting-my-sigint-signal
    -- posix.signal(posix.SIGINT, function(signum)
    --     os.exit(0)
    -- end)
    -- 
    -- posix.signal(posix.SIGTERM, function(signum)
    --     os.exit(0)
    -- end)

    return uhppote.listen(handler, onerror)
end


local commands = {
   commands = {     
       ["get-all-controllers"] = get_all_controllers,
       ["get-controller"] = get_controller,
       ["set-ip"] = set_ip,
       ["get-status"] = get_status,
       ["get-time"] = get_time,
       ["set-time"] = set_time,
       ["listen"] = listen,
   },
}


function commands.exec(cmd, args)
    local f = commands.commands[cmd]
    if f == nil then
        error("unknown command:" .. cmd)
    end
    
    print()
    print(">> " .. cmd)
    print()

    local response = f(args)

    if cmd == "get-all-controllers" then
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
        print(string.format(fmt,f) .. tostring(v[f]))
    end

    print()
end

function parse(args,key,defval)
    if args[key] ~= nil then
        return args[key]
    end

    return defval
end

return commands
