local udp = {}
local socket = require "socket"
local READ <const> = 5 -- seconds
local READALL <const> = 2.5 -- seconds

local debug = false
local bind_address = "*"
local broadcast_address = "255.255.255.255"
local broadcast_port = 60000

function udp.set_bind_addr(address)
    bind_address = address
end

function udp.set_debug(enabled)
    debug = enabled
end

function udp.broadcast(request) 
    local udp = socket.udp4()
    local ok, result = pcall(function() return broadcast(udp, request) end)

    udp:close()

    if not ok then 
        error(result)
    else
        return result
    end
end

function udp.send(request) 
    local udp = socket.udp4()
    local ok, result = pcall(function() return send(udp, request) end)

    udp:close()

    if not ok then 
        error(result)
    else
        return result
    end
end

function broadcast(udp, request) 
    dump(request)
    
    if udp:setsockname(bind_address,0) ~= 1 then
        error("error binding to address")
    end
    
    udp:settimeout(READ,'b')
    udp:settimeout(READALL,'t')

    if udp:setoption('broadcast',true) ~= 1 then
        error("error setting SO_BROADCAST")
    end
    
    udp:sendto(request, broadcast_address,broadcast_port)

    return read_all(udp)
end

function send(udp, request) 
    dump(request)
    
    if udp:setsockname(bind_address,0) ~= 1 then
        error("error binding to address")
    end
    
    udp:settimeout(READ,'b')
    udp:settimeout(READALL,'t')

    if udp:setoption('broadcast',true) ~= 1 then
        error("error setting SO_BROADCAST")
    end
    
    udp:sendto(request, broadcast_address,broadcast_port)

    -- set-ip doesn't return a reply
    if string.byte(request,2) ==  0x96 then
        return {}, nil
    end 

    return read(udp)
end

function read_all(udp)
    local replies = {}

    while true do
        local packet = udp:receive(1024)
        if not packet then
            break
        elseif #packet == 64 then
            dump(packet)
            table.insert(replies, packet)
        end
    end

    return replies
end

function read(udp)
    while true do
        local packet = udp:receive(1024)
        if not packet then
            break
        elseif #packet == 64 then
            dump(packet)
            return packet
        end
    end

    return nil
end

function dump(packet)
    if debug then
        local ix = 1
        for i=0,3 do 
            io.write(string.format("   %08x ",i*16))
            for j=1,8 do
                io.write(string.format(" %02x",string.byte(packet,ix)))
                ix = ix + 1
            end
            io.write(" ")
            for j=1,8 do
                io.write(string.format(" %02x",string.byte(packet,ix)))
                ix = ix + 1
            end
            io.write("\n")
        end
        io.write("\n")
    end
end

return udp