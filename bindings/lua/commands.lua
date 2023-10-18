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
local EVENT_INDEX = 37
local TIME_PROFILE_ID = 29

local TASKS = {
    [0] = "door-controlled",
    [1] = "door-normally-open",
    [2] = "door-normally-closed",
    [3] = "disable-time-profile",
    [4] = "enable-time-profile",
    [5] = "card-no-password",
    [6] = "card-in-password",
    [7] = "card+password",
    [8] = "enable-more-cards",
    [9] = "disable-more-cards",
    [10] = "trigger-once",
    [11] = "disable-pushbutton",
    [12] = "enable-pushbutton"
}

local INTERLOCKS = {
    [0] = "none",
    [1] = "1&2",
    [2] = "3&4",
    [3] = "1&2,3&4",
    [4] = "1&2&3",
    [8] = "1&2&3&4",
}

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
    local controller = parse(args, "controller", CONTROLLER)
    local door = parse(args, "door", DOOR)

    return uhppote.get_door_control(controller, door)
end

function set_door_control(args)
    local controller = parse(args, "controller", CONTROLLER)
    local door = parse(args, "door", DOOR)
    local mode = parse(args, "mode", DOOR_MODE)
    local delay = parse(args, "delay", DOOR_DELAY)

    if mode == "normally-open" then
        mode = 1
    elseif mode == "normally-closed" then
        mode = 2
    elseif mode == "controlled" then
        mode = 3
    end

    return uhppote.set_door_control(controller, door, mode, delay)
end

function open_door(args)
    local controller = parse(args, "controller", CONTROLLER)
    local door = parse(args,"door",DOOR)

    return uhppote.open_door(controller, door)
end

function get_cards(args) 
    local controller = parse(args,"controller", CONTROLLER)

    return uhppote.get_cards(controller)
end

function get_card(args)
    local controller = parse(args, "controller", CONTROLLER)
    local card = parse(args, "card", CARD)

    return uhppote.get_card(controller, card)
end

function get_card_by_index(args)
    local controller = parse(args, "controller", CONTROLLER)
    local index = parse(args, "index", CARD_INDEX)

    return uhppote.get_card_by_index(controller, index)
end

function put_card(args)
    local controller = parse(args, "controller", CONTROLLER)
    local card = parse(args, "card", CARD)
    local start_date = parse(args, "start_date", CARD_START_DATE)
    local end_date = parse(args, "end_date", CARD_END_DATE)
    local doors = parse(args, "doors", CARD_DOORS)
    local PIN = parse(args, "PIN", CARD_PIN)

    local permissions = { 0,0,0,0 }
    for v in doors:gmatch("([^, ]+)") do
        local door, profile = v:match("^([1234]):(%d+)$")

        if door and profile then
          permissions[tonumber(door)] = tonumber(profile)
        elseif v:match("^[1234]$") then
          permissions[tonumber(v)] = 1
        end
    end

    return uhppote.put_card(controller, 
                            card, 
                            start_date, end_date, 
                            permissions[1], permissions[2], permissions[3], permissions[4], 
                            PIN)
end

function delete_card(args)
    local controller = parse(args,"controller",CONTROLLER)
    local card = parse(args,"card",CARD)

    return uhppote.delete_card(controller, card)
end

function delete_all_cards(args)
    local controller = parse(args,"controller",CONTROLLER)

    return uhppote.delete_all_cards(controller)
end

function get_event(args)
    local controller = parse(args,"controller",CONTROLLER)
    local index = parse(args,"index",EVENT_INDEX)

    return uhppote.get_event(controller, index)
end

function get_event_index(args)
    local controller = parse(args,"controller",CONTROLLER)

    return uhppote.get_event_index(controller)
end

function set_event_index(args)
    local controller = parse(args,"controller",CONTROLLER)
    local index = parse(args,"index",EVENT_INDEX)

    return uhppote.set_event_index(controller, index)
end

function record_special_events(args)
    local controller = parse(args,"controller",CONTROLLER)
    local disabled = parse(args,"disabled",false)

    return uhppote.record_special_events(controller, not disabled)
end

function get_time_profile(args)
    local controller = parse(args,"controller",CONTROLLER)
    local profile = parse(args,"profile",TIME_PROFILE_ID)

    return uhppote.get_time_profile(controller, profile)
end

function set_time_profile(args)
    local controller = parse(args,"controller",CONTROLLER)
    local profile = parse(args,"profile",TIME_PROFILE_ID)
    local start_date = parse(args,"start_date","2023-01-01")
    local end_date = parse(args,"end_date","2099-01-01")
    local linked_profile = tonumber(parse(args,"linked","0"))

    local weekdays = string.lower(parse(args, "weekdays", ""))
    local monday = string.match(weekdays,"mon")
    local tuesday = string.match(weekdays,"tue")
    local wednesday = string.match(weekdays,"wed")
    local thursday = string.match(weekdays,"thu")
    local friday = string.match(weekdays,"fri")
    local saturday = string.match(weekdays,"sat")
    local sunday = string.match(weekdays,"sun")

    local segments = { { from = "00:00", to = "00:00" },
                       { from = "00:00", to = "00:00" },
                       { from = "00:00", to = "00:00" } }

    local ix = 1
    for p,q in string.gmatch(parse(args, "segments", ""), "(%d?%d:%d%d)%-(%d?%d:%d%d)") do
        segments[ix].from = p
        segments[ix].to = q
        ix = ix + 1
     end

    return uhppote.set_time_profile(controller, profile,
                                    start_date, end_date,
                                    monday, tuesday, wednesday, thursday, friday, saturday, sunday,
                                    segments[1].from, segments[1].to,
                                    segments[2].from, segments[2].to,
                                    segments[3].from, segments[3].to,
                                    linked_profile)
end

function delete_all_time_profiles(args)
    local controller = parse(args,"controller",CONTROLLER)

    return uhppote.delete_all_time_profiles(controller)
end

function add_task(args)
    local controller = parse(args,"controller",CONTROLLER)
    local task = parse(args,"task",2)
    local start_date = parse(args,"start_date","2023-01-01")
    local end_date = parse(args,"end_date","2099-01-01")
    local at = string.match(parse(args, "at", "00:00"),"(%d?%d:%d%d)") or "12:34"
    local door = tonumber(parse(args,"door","0"))
    local weekdays = string.lower(parse(args, "weekdays", ""))
    local more_cards = tonumber(parse(args,"more_cards","0"))

    local monday = string.match(weekdays,"mon")
    local tuesday = string.match(weekdays,"tue")
    local wednesday = string.match(weekdays,"wed")
    local thursday = string.match(weekdays,"thu")
    local friday = string.match(weekdays,"fri")
    local saturday = string.match(weekdays,"sat")
    local sunday = string.match(weekdays,"sun")
    local task = string.lower(parse(args, "task", ""))

    for k,v in pairs(TASKS) do
      if v == task then
          return uhppote.add_task(controller, 
                                  start_date, end_date,
                                  monday, tuesday, wednesday, thursday, friday, saturday, sunday,
                                  at,
                                  door,
                                  k,
                                  more_cards)
      end
    end

    error("invalid task")
end

function refresh_tasklist(args)
    local controller = parse(args,"controller",CONTROLLER)

    return uhppote.refresh_tasklist(controller)
end

function clear_tasklist(args)
    local controller = parse(args,"controller",CONTROLLER)

    return uhppote.clear_tasklist(controller)
end

function set_pc_control(args)
    local controller = parse(args,"controller",CONTROLLER)
    local disabled = parse(args,"disabled",false)

    return uhppote.set_pc_control(controller, not disabled)
end

function set_interlock(args)
    local controller = parse(args,"controller",CONTROLLER)
    local interlock = parse(args, "interlock", "")

    for k,v in pairs(INTERLOCKS) do
      if v == interlock then
          return uhppote.set_interlock(controller, k)
      end
    end

    error ("invalid interlock")
end

function activate_keypads(args)
    local controller = parse(args,"controller",CONTROLLER)
    local keypads = parse(args, "keypads", "")

    local reader1 = false
    local reader2 = false
    local reader3 = false
    local reader4 = false

    for reader in string.gmatch(keypads, "([1234])[, ]*") do
        if reader == "1" then
          reader1 = true
        elseif reader == "2" then
          reader2 = true
        elseif reader == "3" then
          reader3 = true
        elseif reader == "4" then
          reader4 = true
        end
    end 

    return uhppote.activate_keypads(controller, reader1, reader2, reader3, reader4)
end

function set_door_passcodes(args)
    local controller = parse(args,"controller",CONTROLLER)
    local door = parse(args, "door", DOOR)
    local passcodes = parse(args, "passcodes", "")

    local codes = { 0, 0, 0, 0 }
    local ix = 1
    for v in string.gmatch(passcodes, "([0-9]+)[, ]*") do
      local code = tonumber(v)

      if code > 0 and code < 1000000 then
         codes[ix] = code
         ix = ix + 1
      end
    end 

    return uhppote.set_door_passcodes(controller, tonumber(door), codes[1], codes[2], codes[3], codes[4])
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

-- stylua: ignore start
local commands = {
   commands = {     
       { ["command"] = "get-all-controllers",      ["f"] = get_all_controllers,      flags = {},             options = {} },
       { ["command"] = "get-controller",           ["f"] = get_controller,           flags = {},             options = { "controller" } },
       { ["command"] = "set-ip",                   ["f"] = set_ip,                   flags = {},             options = { "controller","address","netmask","gateway" } },
       { ["command"] = "get-time",                 ["f"] = get_time,                 flags = {},             options = { "controller" } },
       { ["command"] = "set-time",                 ["f"] = set_time,                 flags = {},             options = { "controller","time" } },
       { ["command"] = "get-listener",             ["f"] = get_listener,             flags = {},             options = { "controller" } },
       { ["command"] = "set-listener",             ["f"] = set_listener,             flags = {},             options = { "controller","address","port" } },
       { ["command"] = "get-status",               ["f"] = get_status,               flags = {},             options = { "controller" } },
       { ["command"] = "get-door-control",         ["f"] = get_door_control,         flags = {},             options = { "controller","door" } },
       { ["command"] = "set-door-control",         ["f"] = set_door_control,         flags = {},             options = { "controller","door","mode","delay" } },
       { ["command"] = "open-door",                ["f"] = open_door,                flags = {},             options = { "controller","door" } },
       { ["command"] = "get-cards",                ["f"] = get_cards,                flags = {},             options = { "controller" } },
       { ["command"] = "get-card",                 ["f"] = get_card,                 flags = {},             options = { "controller","card" } },
       { ["command"] = "get-card-by-index",        ["f"] = get_card_by_index,        flags = {},             options = { "controller","index" } },
       { ["command"] = "put-card",                 ["f"] = put_card,                 flags = {},             options = { "controller","card","start-date","end-date","doors","PIN" } },
       { ["command"] = "delete-card",              ["f"] = delete_card,              flags = {},             options = { "controller","card" } },
       { ["command"] = "delete-all-cards",         ["f"] = delete_all_cards,         flags = {},             options = { "controller" } },
       { ["command"] = "get-event",                ["f"] = get_event,                flags = {},             options = { "controller","index" } },
       { ["command"] = "get-event-index",          ["f"] = get_event_index,          flags = {},             options = { "controller" } },
       { ["command"] = "set-event-index",          ["f"] = set_event_index,          flags = {},             options = { "controller", "index" } },
       { ["command"] = "record-special-events",    ["f"] = record_special_events,    flags = { "disabled" }, options = { "controller" } },
       { ["command"] = "get-time-profile",         ["f"] = get_time_profile,         flags = {},             options = { "controller", "profile" } },
       { ["command"] = "set-time-profile",         ["f"] = set_time_profile,         flags = {},             options = { "controller", "profile", "start-date", "end-date", "weekdays", "segments", "linked" } },
       { ["command"] = "delete-all-time-profiles", ["f"] = delete_all_time_profiles, flags = {},             options = { "controller", "profile" } },
       { ["command"] = "add-task",                 ["f"] = add_task,                 flags = {},             options = { "controller", "door", "task", "at", "start-date", "end-date", "weekdays", "more-cards" } },
       { ["command"] = "refresh-tasklist",         ["f"] = refresh_tasklist,         flags = {},             options = { "controller" } },
       { ["command"] = "clear-tasklist",           ["f"] = clear_tasklist,           flags = {},             options = { "controller" } },
       { ["command"] = "set-pc-control",           ["f"] = set_pc_control,           flags = { "disabled" }, options = { "controller" } },
       { ["command"] = "set-interlock",            ["f"] = set_interlock,            flags = {},             options = { "controller", "interlock" } },
       { ["command"] = "activate_keypads",         ["f"] = activate_keypads,         flags = {},             options = { "controller", "keypads" } },
       { ["command"] = "set-door-passcodes",       ["f"] = set_door_passcodes,       flags = {},             options = { "controller", "door", "passcodes" } },
       { ["command"] = "listen",                   ["f"] = listen,                   flags = {},             options = { "controller" } },
   },
}
-- stylua: ignore end

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