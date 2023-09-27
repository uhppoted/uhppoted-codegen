local udp = {}

debug = false

function udp.set_debug(enabled)
    debug = enabled
end

function udp.broadcast(request) 
    dump(request)
    
    -- socket, err := net.ListenUDP("udp", bindAddr)
    -- if err != nil {
    --     return nil, err
    -- }
    -- 
    -- defer socket.Close()
    -- 
    -- if err := socket.SetWriteDeadline(time.Now().Add(WRITE_TIMEOUT)); err != nil {
    --     return nil, err
    -- }
    -- 
    -- if err := socket.SetReadDeadline(time.Now().Add(READ_TIMEOUT)); err != nil {
    --     return nil, fmt.Errorf("Failed to set UDP read timeout [%v]", err)
    -- }
    -- 
    -- if _, err := socket.WriteToUDP(request, destAddr); err != nil {
    --     return nil, err
    -- }
    -- 
    -- return readAll(socket)

end

function dump(packet)
    if debug then
        ix = 1
        for i=0,3 do 
            io.write(string.format("   %08x ",i*16))
            for j=1,8 do
                io.write(string.format(" %02x",packet[ix]))
                ix = ix + 1
            end
            io.write(" ")
            for j=1,8 do
                io.write(string.format(" %02x",packet[ix]))
                ix = ix + 1
            end
            io.write("\n")
        end
        io.write("\n")
    end
end

return udp