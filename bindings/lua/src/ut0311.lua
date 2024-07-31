local ut0311 = {}
local socket = require("socket")
local READ <const> = 5 -- seconds
local READALL <const> = 2.5 -- seconds

local debug = false
local bind_address = "*"
local bind_port = 0
local broadcast_address = "255.255.255.255"
local broadcast_port = 60000
local listen_address = "*"
local listen_port = 60001

function ut0311.set_bind_address(addr)
    if not addr or addr == "" then
        bind_address = "*"
        bind_port = 0
    else
        local address, port = addr:match("^(.-):(%d*)$")

        if address and port and address ~= "" and port ~= "" then
            bind_address = address
            bind_port = tonumber(port)
        elseif address and port and port ~= "" then
            bind_address = "*"
            bind_port = tonumber(port)
        else
            bind_address = addr
            bind_port = 0
        end
    end
end

function ut0311.set_broadcast_address(addr)
    if not addr or addr == "" then
        broadcast_address = "255.255.255.255"
        broadcast_port = 60000
    else
        local address, port = addr:match("^(.-):(%d*)$")

        if address and port and address ~= "" and port ~= "" then
            broadcast_address = address
            broadcast_port = tonumber(port)
        elseif address and port and port ~= "" then
            broadcast_address = "255.255.255.255"
            broadcast_port = tonumber(port)
        else
            broadcast_address = addr
            broadcast_port = 60000
        end
    end
end

function ut0311.set_listen_address(addr)
    if not addr or addr == "" then
        listen_address = "*"
        listen_port = 60001
    else
        local address, port = addr:match("^(.-):(%d*)$")

        if address and port and address ~= "" and port ~= "" then
            listen_address = address
            listen_port = tonumber(port)
        elseif address and port and port ~= "" then
            listen_address = "*"
            listen_port = tonumber(port)
        else
            listen_address = addr
            listen_port = 60001
        end
    end
end

function ut0311.set_debug(enabled)
    debug = enabled
end

function ut0311.broadcast(request)
    local sock = socket.udp4()
    local ok, result = pcall(function()
        return broadcast(sock, request)
    end)

    sock:close()

    if not ok then
        error(result)
    else
        return result
    end
end

function ut0311.send(controller, request)
    if controller['address'] ~= "" and controller['transport'] == "tcp" then
        return tcp_sendto(controller['address'], request)
    end
    
    if controller['address'] ~= "" then
        return udp_sendto(controller['address'], request)
    end
       
    return udp_broadcast_to(request)
end

function ut0311.listen(handler)
    local f = function(packet)
        dump(packet)
        handler(packet)
    end

    local sock = socket.udp4()
    local ok, result = pcall(function()
        return listen_to(sock, f)
    end)

    sock:close()

    if not ok then
        error(result)
    else
        return result
    end
end

function broadcast(sock, request)
    dump(request)

    if sock:setsockname(bind_address, bind_port) ~= 1 then
        error("error binding to address")
    end

    sock:settimeout(READ, "b")
    sock:settimeout(READALL, "t")

    if sock:setoption("broadcast", true) ~= 1 then
        error("error setting SO_BROADCAST")
    end

    sock:sendto(request, broadcast_address, broadcast_port)

    return read_all(sock)
end

function udp_broadcast_to(request)
    local sock = socket.udp4()
    local ok, result = pcall(function()
        return send(sock, request)
    end)

    sock:close()

    if not ok then
        error(result)
    else
        return result
    end
end

function udp_sendto(address, request)
    dump(request)

    addr,port = addrport(address)

    local sock = socket.udp4()
    local ok, result = pcall(function()
        if sock:setsockname(bind_address, 0) ~= 1 then
            error("error binding to address")
        end

        sock:settimeout(READ, "b")
        sock:settimeout(READALL, "t")
        sock:setpeername('192.168.1.100',60000)
        sock:send(request)

        -- set-ip doesn't return a reply
        if string.byte(request, 2) == 0x96 then
            return {}, nil
        end

        local packet = sock:receive(1024)
        if packet and #packet == 64 then
            dump(packet)
            return packet
        end

        return nil
    end)

    sock:close()

    if not ok then
        error(result)
    else
        return result
    end
end

function tcp_sendto(address, request)
    dump(request)

    addr,port = addrport(address)

    local sock = socket.tcp4()
    local ok, result = pcall(function()
        if sock:connect(addr, port) ~= 1 then
            error("error connecting to ", addr, port)
        end

        sock:settimeout(READ, "b")
        sock:settimeout(READALL, "t")
        sock:send(request)

        -- set-ip doesn't return a reply
        if string.byte(request, 2) == 0x96 then
            return {}, nil
        end

        local packet = sock:receive('*a')
        if packet and #packet == 64 then
            dump(packet)
            return packet
        end


        return nil
    end)

    sock:close()

    if not ok then
        error(result)
    else
        return result
    end
end

function send(sock, request)
    dump(request)

    if sock:setsockname(bind_address, 0) ~= 1 then
        error("error binding to address")
    end

    sock:settimeout(READ, "b")
    sock:settimeout(READALL, "t")

    if sock:setoption("broadcast", true) ~= 1 then
        error("error setting SO_BROADCAST")
    end

    sock:sendto(request, broadcast_address, broadcast_port)

    -- set-ip doesn't return a reply
    if string.byte(request, 2) == 0x96 then
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
    if sock:setsockname(listen_address, listen_port) ~= 1 then
        error("error binding to listen address")
    end

    sock:settimeout(0, "b")
    sock:settimeout(nil, "t")

    local recv = function(sock)
        local packet, status = sock:receive(1024)
        if status == "timeout" then
            coroutine.yield(sock)
        end
        return packet, status
    end

    local co = coroutine.create(function ()
        while true do
            local packet,status = recv(sock)
            if status == "closed" then
                break
            elseif packet and #packet == 64 then
                f(packet)
            end
        end
    end)

    while true do
        local status, res = coroutine.resume(co)
        if not res then
            print("AWOOGAH")
            break
        end
        socket.select({sock})
    end
end

function addrport(address)
    if address and address ~= "" then
        local addr, port = address:match("^(.-):(%d*)$")
        if addr and port and addr ~= "" and port ~= "" then
            return addr, tonumber(port)
        end

        return address, 60000
    end

    return "255.255.255.255", 60000
end

function dump(packet)
    if debug then
        local ix = 1
        for i = 0, 3 do
            io.write(string.format("   %08x ", i * 16))
            for j = 1, 8 do
                io.write(string.format(" %02x", string.byte(packet, ix)))
                ix = ix + 1
            end
            io.write(" ")
            for j = 1, 8 do
                io.write(string.format(" %02x", string.byte(packet, ix)))
                ix = ix + 1
            end
            io.write("\n")
        end
        io.write("\n")
    end
end

return ut0311
