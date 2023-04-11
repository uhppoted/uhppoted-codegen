<?php

function get_controller_request($deviceID) {
    $packet = array_fill(0, 64, 0x00);

    $packet[0] = 0x17;
    $packet[1] = 0x94;

    $packet = pack_uint32($deviceID, $packet, 4);

    return $packet;
}

function set_ip_request($deviceID,$address,$netmask,$gateway) {
    $packet = array_fill(0, 64, 0x00);

    $packet[0] = 0x17;
    $packet[1] = 0x96;

    $packet = pack_uint32($deviceID, $packet, 4);
    $packet = pack_IPv4($address, $packet, 8);
    $packet = pack_IPv4($netmask, $packet, 12);
    $packet = pack_IPv4($gateway, $packet, 16);

    return $packet;
}


function pack_uint32($v,$packet,$offset) {
    $packet[$offset]   = ($v >> 0)  & 0x00ff;
    $packet[$offset+1] = ($v >> 8)  & 0x00ff;
    $packet[$offset+2] = ($v >> 16) & 0x00ff;
    $packet[$offset+3] = ($v >> 24) & 0x00ff;

    return $packet;
}

function pack_IPv4($address, $packet, $offset) {
    $addr = ip2long($address);

     if (!$addr) {
        throw new Exception(sprintf('invalid IPv4 address (%)', $address));        
     }

    $packet[$offset]   = ($addr >> 24) & 0x00ff;
    $packet[$offset+1] = ($addr >> 16) & 0x00ff;
    $packet[$offset+2] = ($addr >> 8)  & 0x00ff;
    $packet[$offset+3] = ($addr >> 0)  & 0x00ff;

    return $packet;
}

?>