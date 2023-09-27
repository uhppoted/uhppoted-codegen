#!/usr/local/bin/lua

local argparse = require "argparse"
local commands = require "commands"

print("uhppoted-codegen: Lua sample application")

local parser = argparse()
   :name "main.lua"
   :description "uhppoted-codegen: Lua sample application"
   :epilog "For more info, see https://github.com/uhppoted/uhppoted-codegen"

parser:flag        "--debug"
      :description "Displays sent and received UDP packets"
      :default     "false"

parser:option      "--bind"
      :description "UDP IPv4 bind address"
      :default     "0.0.0.0"

parser:option      "--broadcast"
      :description "UDP IPv4 broadcast address"
      :default     "255.255.255.255:60000"

parser:option "--listen"
      :description "UDP IPv4 listen address"
      :default     "0.0.0.0:60001"

parser:argument "command"

local args = parser:parse()
local command = args["command"]

if command == "all" then
    print(">> ALL")
elseif commands[command] then
    print(">> " .. command)
    f = commands[command]
    commands.exec(f)
else
    print()
    print("   *** ERROR: invalid command: " .. command)
    print()
end
