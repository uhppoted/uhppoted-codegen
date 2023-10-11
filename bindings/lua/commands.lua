local uhppote = require("src/uhppote")
local os = require("os")

local CONTROLLER <const> = 405419896
local ADDRESS <const> = "192.168.1.100"
local NETMASK <const> = "255.255.255.0"
local GATEWAY <const> = "192.168.1.1"
local LISTENER <const> = { ["address"] = "192.168.1.100", ["port"] = 60001 }
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

function get_time(args)
    local controller = parse(args,"controller",CONTROLLER)

    return uhppote.get_time(controller)
end

function set_time(args)
    local controller = parse(args,"controller",CONTROLLER)
    local datetime = parse(args,"time",os.date("%Y-%m-%d %H:%M:%S"))

    return uhppote.set_time(controller,datetime)
end

function get_listener(args)
    local controller = parse(args,"controller",CONTROLLER)

    return uhppote.get_listener(controller)
end

function set_listener(args)
    local controller = parse(args,"controller",CONTROLLER)
    local address = parse(args,"address",LISTENER.address)
    local port = tonumber(parse(args,"port",LISTENER.port))

    return uhppote.set_listener(controller,address,port)
end


function get_status(args)
    local controller = parse(args,"controller",CONTROLLER)

    return uhppote.get_status(controller)
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
       { ["command"] = "get-all-controllers", ["f"] = get_all_controllers, options = {} },
       { ["command"] = "get-controller",      ["f"] = get_controller,      options = { "controller" } },
       { ["command"] = "set-ip",              ["f"] = set_ip,              options = { "controller","address","netmask","gateway" } },
       { ["command"] = "get-time",            ["f"] = get_time,            options = { "controller" } },
       { ["command"] = "set-time",            ["f"] = set_time,            options = { "controller","time" } },
       { ["command"] = "get-listener",        ["f"] = get_listener,        options = { "controller" } },
       { ["command"] = "set-listener",        ["f"] = set_listener,        options = { "controller","address","port" } },
       { ["command"] = "get-status",          ["f"] = get_status,          options = { "controller" } },
       { ["command"] = "listen",              ["f"] = listen,              options = { "controller" } },
   },
}


function commands.exec(cmd, args)
    for _,c in ipairs(commands.commands) do
        if c.command == cmd then
           exec(c,args)
           return
        end
    end

    error("unknown command:" .. cmd)
end

function exec(cmd, args)
    print()
    print(">> " .. cmd.command)
    print()

    local response = cmd.f(args)

    if cmd.command == "get-all-controllers" then
       for _, v in ipairs(response) do
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
