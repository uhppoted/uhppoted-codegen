local uhppote = require("src/uhppote")

function get_all_controllers()
   return uhppote.get_all_controllers()
end

local commands = {
   ["get-all-controllers"] = get_all_controllers,
}

function commands.exec(cmd)
   response = cmd()
   pprint(response)
end

function pprint(v)
   print(v)
end

return commands
