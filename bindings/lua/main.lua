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
parser:command_target("command")
for _,v in ipairs(commands.commands) do
    local cmd = parser:command(v.command)
    for _,option in ipairs(v.options) do
        cmd:option(string.format("--%s",option))
    end
end

local args = parser:parse()
local command = args["command"]
local debug = args["debug"]

uhppote.set_bind_address(args["bind"])
uhppote.set_broadcast_address(args["broadcast"])
uhppote.set_listen_address(args["listen"])
uhppote.set_debug(debug)

if command == "all" then
    print()
    for _,v in ipairs(commands.commands) do
        if v.command ~= "listen" then
            commands.exec(v.command, args)
        end
    end
    return
else
    commands.exec(command, args)
end

