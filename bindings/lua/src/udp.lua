local udp = {}
local socket = require "socket"
local READ <const> = 5 -- seconds
local READALL <const> = 2.5 -- seconds

local debug = false
local bind_address = "*"
local broadcast_address = "255.255.255.255"
local broadcast_port = 60000
local listen_address = "*"
local listen_port = 60001

function udp.set_bind_addr(address)
    bind_address = address
end

function udp.set_debug(enabled)
    debug = enabled
end

function udp.broadcast(request) 
    local sock = socket.udp4()
    local ok, result = pcall(function() return broadcast(sock, request) end)

    sock:close()

    if not ok then 
        error(result)
    else
        return result
    end
end

function udp.send(request) 
    local sock = socket.udp4()
    local ok, result = pcall(function() return send(sock, request) end)

    sock:close()

    if not ok then 
        error(result)
    else
        return result
    end
end

function udp.listen(handler)
    local f = function(packet)
              dump(packet)
              handler(packet)
              end

    local sock = socket.udp4()
    local ok, result = pcall(function() return listen_to(sock, f) end)

    sock:close()

    if not ok then 
        error(result)
    else
        return result
    end
end

    -- events := make(chan []byte)
    -- e := make(chan error)

    -- go func() {
    --     buffer := make([]byte, 1024)

    --     for {
    --         if N, _, err := socket.ReadFromUDP(buffer); err != nil {
    --             e <- err
    --         } else if N == 64 {
    --             m := make([]byte, 64)
    --             copy(m, buffer[0:N])
    --             events <- m
    --         }
    --     }
    -- }()

    -- for {
    --     select {
    --     case m := <-events:
    --         dump(m)
    --         ch <- m

    --     case err := <-e:
    --         return err
    --     }
    -- }

function broadcast(sock, request) 
    dump(request)
    
    if sock:setsockname(bind_address,0) ~= 1 then
        error("error binding to address")
    end
    
    sock:settimeout(READ,'b')
    sock:settimeout(READALL,'t')

    if sock:setoption('broadcast',true) ~= 1 then
        error("error setting SO_BROADCAST")
    end
    
    sock:sendto(request, broadcast_address,broadcast_port)

    return read_all(sock)
end

function send(sock, request) 
    dump(request)
    
    if sock:setsockname(bind_address,0) ~= 1 then
        error("error binding to address")
    end
    
    sock:settimeout(READ,'b')
    sock:settimeout(READALL,'t')

    if sock:setoption('broadcast',true) ~= 1 then
        error("error setting SO_BROADCAST")
    end
    
    sock:sendto(request, broadcast_address,broadcast_port)

    -- set-ip doesn't return a reply
    if string.byte(request,2) ==  0x96 then
        return {}, nil
    end 

    return read(sock)
end

function read_all(sock)
    local replies = {}

    while true do
        local packet = sock:receive(1024)
        if not packet then
            break
        elseif #packet == 64 then
            dump(packet)
            table.insert(replies, packet)
        end
    end

    return replies
end

function read(sock)
    while true do
        local packet = sock:receive(1024)
        if not packet then
            break
        elseif #packet == 64 then
            dump(packet)
            return packet
        end
    end

    return nil
end

function listen_to(sock, f)
    if sock:setsockname(listen_address,listen_port) ~= 1 then
        error("error binding to listen address")
    end
    
    sock:settimeout(nil,'b')
    sock:settimeout(nil,'t')

    while true do
        local packet = sock:receive(1024)
        if not packet then
            break
        elseif #packet == 64 then
            f(packet)
        end
    end
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