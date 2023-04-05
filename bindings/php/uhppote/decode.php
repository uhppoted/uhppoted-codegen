<?php

class GetControllerResponse {
    public $controller;
    public $ip_address;
    public $subnet_mask;
    public $gateway;
    public $MAC_address;
    public $version;
    public $date;
}

function get_controller_response($packet) {
    if (count($packet) != 64) {
        throw new Exception(sprintf('invalid reply packet length (%d)', count($packet)));
    }

    // Ref. v6.62 firmware event
    if ($packet[0] != 0x17 && ($packet[0] != 0x19 || $packet[1] != 0x20)) {
    var_dump($packet,0,8);
        throw new Exception(sprintf('invalid reply start of message byte (%02x)', $packet[0]));
    }

    if ($packet[1] != 0x94) {
        throw new Exception(sprintf('invalid reply function code (%02x)', $packet[1]));
    }

    $response = new GetControllerResponse();
    
    $response->controller = unpack_uint32($packet, 4);
    $response->ip_address = unpack_IPv4($packet, 8);
    $response->subnet_mask = unpack_IPv4($packet, 12);
    $response->gateway = unpack_IPv4($packet, 16);
    $response->MAC_address = unpack_MAC($packet, 20);
    $response->version = unpack_version($packet, 26); 
    $response->date = unpack_date($packet, 28); 

    return $response;
}

function unpack_uint32($packet,$offset) {
    $v = 0;

    $v = $packet[$offset+3] & 0x00ff;
    $v <<= 8;
    $v |= $packet[$offset+2] & 0x00ff;
    $v <<= 8;
    $v |= $packet[$offset+1] & 0x00ff;
    $v <<= 8;
    $v |= $packet[$offset] & 0x00ff;

    return $v;
}

function unpack_IPv4($packet,$offset) {
    return sprintf('%d.%d.%d.%d',$packet[$offset],
                                 $packet[$offset+1],
                                 $packet[$offset+2],
                                 $packet[$offset+3]);
}

function unpack_MAC($packet,$offset) {
    return sprintf('%02x:%02x:%02x:%02x:%02x:%02x', $packet[$offset],
                                                    $packet[$offset+1],
                                                    $packet[$offset+2],
                                                    $packet[$offset+3],
                                                    $packet[$offset+4],
                                                    $packet[$offset+5]);
}

function unpack_version($packet,$offset) {
    $major = $packet[$offset];
    $minor = $packet[$offset+1];

    return sprintf("v%x.%'.02x", $major, $minor);
}

function unpack_date($packet, $offset) {
    $bcd = bcd2string(array_slice($packet,$offset,4));
    $year = substr($bcd,0,4);
    $month = substr($bcd,4,2);
    $day = substr($bcd,6,2);

    return sprintf('%s-%s-%s',$year,$month,$day);
}

function bcd2string ($bytes) {
  return join('',array_map(fn($v) => sprintf('%02x',$v),$bytes));
}

?>