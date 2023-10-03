#!/usr/local/bin/lua

local argparse = require "argparse"
local commands = require "commands"
local uhppote = require("src/uhppote")

print("uhppoted-codegen: Lua sample application")

local parser = argparse()
   :name "main.lua"
   :description "uhppoted-codegen: Lua sample application"
   :epilog "For more info, see https://github.com/uhppoted/uhppoted-codegen"

parser:flag        "--debug"
      :description "Displays sent and received UDP packets"

parser:option      "--bind"
      :description "UDP IPv4 bind address"
      :default     "0.0.0.0"

parser:option      "--broadcast"
      :description "UDP IPv4 broadcast address"
      :default     "255.255.255.255:60000"

parser:option "--listen"
      :description "UDP IPv4 listen address"
      :default     "0.0.0.0:60001"

parser:command "all"
for k,v in pairs(commands.commands) do
    parser:command(k)
end

local args = parser:parse()
local command = args["command"]
local debug = args["debug"]

uhppote.set_debug(debug)

if args["all"] then
    print()
    for k,v in pairs(commands.commands) do
        if k ~= "listen" then
            print(">> " .. k)
            print()
            local f = commands.commands[k]
            commands.exec(f)
        end
    end
    return
end

for k,v in pairs(commands.commands) do
    if args[k] then
        print()
        print(">> " .. k)
        print()
        local f = commands.commands[k]
        commands.exec(f)
        return
    end
end

os.exit(1)
