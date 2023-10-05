local decode = {}
local structs = require("src/structs")

function decode.get_controller_response(packet) 
    if (#packet ~= 64) then
        error("invalid reply packet length (" .. #packet .. ")")
    end

    --  Ref. v6.62 firmware event
    if (string.byte(packet,1) ~= 0x17 and (string.byte(packet,1) ~= 0x19 or string.byte(packet,2) ~= 0x20)) then
        error(string.format("invalid reply start of message byte (%02x)",string.byte(packet,1)))
    end

    if string.byte(packet,2) ~= 0x94 then
        error(string.format("invalid reply function code (%02x)", string.byte(packet,2)))
    end

    local controller = unpack_uint32(packet, 4)
    local address = unpack_IPv4(packet, 8)
    local netmask = unpack_IPv4(packet, 12)
    local gateway = unpack_IPv4(packet, 16)
    local MAC = unpack_MAC(packet, 20)
    local version = unpack_version(packet, 26)
    local date = unpack_date(packet, 28)

    return structs.get_controller_response(controller,address,netmask,gateway,MAC,version,date)
end

function decode.event(packet) 
    if (#packet ~= 64) then
        error("invalid reply packet length (" .. #packet .. ")")
    end

    --  Ref. v6.62 firmware event
    if (string.byte(packet,1) ~= 0x17 and (string.byte(packet,1) ~= 0x19 or string.byte(packet,2) ~= 0x20)) then
        error(string.format("invalid reply start of message byte (%02x)",string.byte(packet,1)))
    end

    if string.byte(packet,2) ~= 0x20 then
        error(string.format("invalid reply function code (%02x)", string.byte(packet,2)))
    end

    local controller = unpack_uint32(packet, 4)

    return structs.event(controller)
end

function unpack_uint32(packet,offset)
    return string.unpack("<I4", packet, offset+1)
end

function unpack_IPv4(packet,offset)
    local address = {string.unpack("BBBB", packet, offset+1)}

    return string.format("%u.%u.%u.%u",address[1],address[2],address[3],address[4])
end

function unpack_MAC(packet,offset)
    local MAC = {string.unpack("BBBBBB", packet, offset+1)}

    return string.format("%02x:%02x:%02x:%02x:%02x:%02x",MAC[1],MAC[2],MAC[3],MAC[4],MAC[5],MAC[6])
end

function unpack_version(packet,offset)
    local version = {string.unpack("BB", packet, offset+1)}
    local major = version[1]
    local minor = version[2]

    return string.format("v%x.%02x",major, minor)
end

function unpack_date(packet,offset)
    local bcd = bcd2string(packet:sub(offset+1,offset+4))
    local year = tonumber(bcd:sub(1, 4))
    local month = tonumber(bcd:sub(5, 6))
    local day = tonumber(bcd:sub(7, 8))

    return string.format("%04u-%02u-%02u", year,month,day)
end

function bcd2string(bytes)
    return bytes:gsub(".", function(c) return string.format("%02x", string.byte(c)) end)
end


return decode


