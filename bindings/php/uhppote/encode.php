<?php

function get_controller_request($deviceID) {
    $packet = array_fill(0, 64, 0x00);

    $packet[0] = 0x17;
    $packet[1] = 0x94;

    $packet = pack_uint32($deviceID, $packet, 4);

    return $packet;
}

function pack_uint32($v,$packet,$offset) {
    $packet[$offset] = ($v >> 0) & 0x00ff;
    $packet[$offset+1] = ($v >> 8) & 0x00ff;
    $packet[$offset+2] = ($v >> 16) & 0x00ff;
    $packet[$offset+3] = ($v >> 24) & 0x00ff;

    return $packet;
}

?>