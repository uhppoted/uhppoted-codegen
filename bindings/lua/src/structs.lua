local structs = {}

function structs.get_controller_response(controller, address,netmask,gateway,MAC,version,date) 
    local response = {
        controller = controller,
        ip_address = address,
        subnet_mask = netmask,
        gateway = gateway,
        MAC_address = MAC,
        version = version,
        date = date,

        fields = function(self) 
                    return { "controller", "ip_address", "subnet_mask", "gateway", "MAC_address", "version", "date" }
                  end
    }

    return response
end

return structs


