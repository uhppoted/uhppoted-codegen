local encode = {}

function encode.get_controller_request(device_id) 
    packet = { 
        0,0,0,0, 0,0,0,0,0, 0,0,0,0, 0,0,0,0,0,
        0,0,0,0, 0,0,0,0,0, 0,0,0,0, 0,0,0,0,0,
        0,0,0,0, 0,0,0,0,0, 0,0,0,0, 0,0,0,0,0,
        0,0,0,0, 0,0,0,0,0, 0,0,0,0, 0,0,0,0,0,
    }

    packet[1] = 0x17
    packet[2] = 0x94

    packet = pack_uint32(device_id, packet, 4)

  return table.concat(packet)
end

function pack_uint32(v,packet,offset)
    bytes = string.pack("<I4", v)
    
    packet[offset+1] = string.byte(bytes,1)
    packet[offset+2] = string.byte(bytes,2)
    packet[offset+3] = string.byte(bytes,3)
    packet[offset+4] = string.byte(bytes,4)
   
    return packet
end

return encode