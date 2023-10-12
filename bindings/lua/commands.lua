local uhppote = require("src/uhppote")
local os = require("os")

local CONTROLLER <const> = 405419896
local ADDRESS <const> = "192.168.1.100"
local NETMASK <const> = "255.255.255.0"
local GATEWAY <const> = "192.168.1.1"
local LISTENER <const> = { ["address"] = "192.168.1.100", ["port"] = 60001 }
local DOOR = 1
local DOOR_MODE = "controlled"
local DOOR_DELAY = 5
local CARD = 10058400
local CARD_INDEX = 1
local CARD_START_DATE = "2023-01-01"
local CARD_END_DATE = "2023-12-31"
local CARD_DOORS = "1,2,3,4"
local CARD_PIN = 0

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

function get_status(args)
    local controller = parse(args,"controller",CONTROLLER)

    return uhppote.get_status(controller)
end

function get_listener(args)
    local controller = parse(args,"controller",CONTROLLER)

    return uhppote.get_listener(controller)
end

function set_listener(args)
    local controller = parse(args,"controller",CONTROLLER)
    local address = parse(args,"address",LISTENER.address)
    local port = parse(args,"port",LISTENER.port)

    return uhppote.set_listener(controller,address,port)
end

function get_door_control(args)
    local controller = parse(args,"controller",CONTROLLER)
    local door = parse(args,"door",DOOR)

    return uhppote.get_door_control(controller,door)
end

function set_door_control(args)
    local controller = parse(args,"controller",CONTROLLER)
    local door = parse(args,"door",DOOR)
    local mode = parse(args,"mode",DOOR_MODE)
    local delay = parse(args,"delay",DOOR_DELAY)

    if mode == "normally-open" then
        mode = 1
    elseif mode == "normally-closed" then
        mode = 2
    elseif mode == "controlled" then
        mode = 3
    end

    return uhppote.set_door_control(controller,door,mode,delay)
end

function open_door(args)
    local controller = parse(args,"controller",CONTROLLER)
    local door = parse(args,"door",DOOR)

    return uhppote.open_door(controller,door)
end

function get_cards(args)
    local controller = parse(args,"controller",CONTROLLER)

    return uhppote.get_cards(controller)
end

function get_card(args)
    local controller = parse(args,"controller",CONTROLLER)
    local card = parse(args,"card",CARD)

    return uhppote.get_card(controller,card)
end

function get_card_by_index(args)
    local controller = parse(args,"controller",CONTROLLER)
    local index = parse(args,"index",CARD_INDEX)

    return uhppote.get_card_by_index(controller,index)
end

function put_card(args)
    local controller = parse(args,"controller",CONTROLLER)
    local card = parse(args,"card",CARD)
    local start_date = parse(args,"start-date",CARD_START_DATE)
    local end_date = parse(args,"end-date",CARD_END_DATE)
    local doors = parse(args,"doors",CARD_DOORS)
    local PIN = parse(args,"PIN",CARD_PIN)

    local door1 = 0
    local door2 = 0
    local door3 = 0
    local door4 = 0

    for v in string.gmatch(doors, "(%d+)") do
        local door = tonumber(v)
        if door == 1 then
           door1 = 1
        elseif door == 2 then
           door2 = 1
        elseif door == 3 then
           door3 = 1
        elseif door == 4 then
           door4 = 1
        end
    end

    return uhppote.put_card(controller,card,start_date, end_date, door1,door2, door3, door4, PIN)
end

function delete_card(args)
    local controller = parse(args,"controller",CONTROLLER)
    local card = parse(args,"card",CARD)

    return uhppote.delete_card(controller,card)
end

function delete_all_cards(args)
    local controller = parse(args,"controller",CONTROLLER)

    return uhppote.delete_all_cards(controller)
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
       { ["command"] = "get-door-control",    ["f"] = get_door_control,    options = { "controller","door" } },
       { ["command"] = "set-door-control",    ["f"] = set_door_control,    options = { "controller","door","mode","delay" } },
       { ["command"] = "open-door",           ["f"] = open_door,           options = { "controller","door" } },
       { ["command"] = "get-cards",           ["f"] = get_cards,           options = { "controller" } },
       { ["command"] = "get-card",            ["f"] = get_card,            options = { "controller","card" } },
       { ["command"] = "get-card-by-index",   ["f"] = get_card_by_index,   options = { "controller","index" } },
       { ["command"] = "put-card",            ["f"] = put_card,            options = { "controller","card","start-date","end-date","doors","PIN" } },
       { ["command"] = "delete-card",         ["f"] = delete_card,         options = { "controller","card" } },
       { ["command"] = "delete-all-cards",    ["f"] = delete_all_cards,    options = { "controller" } },
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
