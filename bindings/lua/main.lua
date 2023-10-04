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
for k,v in pairs(commands.commands) do
    local cmd = parser:command(k)
    cmd:option "--controller"
end

local args = parser:parse()
local command = args["command"]
local debug = args["debug"]

uhppote.set_debug(debug)

if command == "all" then
    print()
    for k,v in pairs(commands.commands) do
        if k ~= "listen" then
            commands.exec(k, args)
        end
    end
    return
else
    commands.exec(command, args)
end

