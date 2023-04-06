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

class Event {
    public $controller;
    public $event_index;
    public $event_type;
    public $event_access_granted;
    public $event_door;
    public $event_direction;
    public $event_card;
    public $event_timestamp;
    public $event_reason;
    public $system_date;
    public $system_time;
    public $system_error;
    public $door1_open;
    public $door2_open;
    public $door3_open;
    public $door4_open;
    public $door1_button;
    public $door2_button;
    public $door3_button;
    public $door_button;
    public $relays;
    public $inputs;
    public $special_info;
    public $sequence_no;
}


function get_controller_response($packet) {
    if (count($packet) != 64) {
        throw new Exception(sprintf('invalid reply packet length (%d)', count($packet)));
    }

    // Ref. v6.62 firmware event
    if ($packet[0] != 0x17 && ($packet[0] != 0x19 || $packet[1] != 0x20)) {
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

function event($packet) {
    if (count($packet) != 64) {
        throw new Exception(sprintf('invalid event packet length (%d)', count($packet)));
    }

    // Ref. v6.62 firmware event
    if ($packet[0] != 0x17 && ($packet[0] != 0x19 || $packet[1] != 0x20)) {
        throw new Exception(sprintf('invalid event start of message byte (%02x)', $packet[0]));
    }

    if ($packet[1] != 0x20) {
        throw new Exception(sprintf('invalid event function code (%02x)', $packet[1]));
    }

    $event = new Event();
    
    $event->controller = unpack_uint32($packet, 4);
    $event->event_index = unpack_uint32($packet, 8);
    $event->event_type = unpack_uint8($packet, 12);
    $event->event_access_granted = unpack_bool($packet, 13);
    $event->event_door = unpack_uint8($packet, 14);
    $event->event_direction = unpack_uint8($packet, 15); 
    $event->event_card = unpack_uint32($packet, 16);
    $event->event_timestamp = unpack_datetime($packet, 20);
    $event->event_reason = unpack_uint8($packet, 27); 
    $event->system_date = unpack_shortdate($packet, 51);
    $event->system_time = unpack_time($packet, 37);
    $event->system_error = unpack_uint8($packet, 36); 
    $event->door1_open = unpack_bool($packet, 28);
    $event->door2_open = unpack_bool($packet, 29);
    $event->door3_open = unpack_bool($packet, 30);
    $event->door4_open = unpack_bool($packet, 31);
    $event->door1_button = unpack_bool($packet, 32);
    $event->door2_button = unpack_bool($packet, 33);
    $event->door3_button = unpack_bool($packet, 34);
    $event->door4_button = unpack_bool($packet, 35);
    $event->relays = unpack_uint8($packet, 49);
    $event->inputs = unpack_uint8($packet, 50);
    $event->special_info  = unpack_uint8($packet, 48);
    $event->sequence_no = unpack_uint32($packet, 40);

    return $event;
}

function unpack_uint8($packet,$offset) {
    return $packet[$offset] & 0x00ff;
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

function unpack_bool($packet,$offset) {
    if ($packet[$offset] & 0x00ff == 1) {
        return true;
    } else {
        return false;
    }
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

function unpack_shortdate($packet, $offset) {
    $bcd = bcd2string(array_slice($packet,$offset,3));
    $year = substr($bcd,0,2);
    $month = substr($bcd,2,2);
    $day = substr($bcd,4,2);

    return sprintf('20%s-%s-%s',$year,$month,$day);
}

function unpack_time($packet, $offset) {
    $bcd = bcd2string(array_slice($packet,$offset,3));
    $hour = substr($bcd,0,2);
    $minute = substr($bcd,2,2);
    $second = substr($bcd,4,2);

    return sprintf('%s:%s:%s',$hour,$minute,$second);
}

function unpack_datetime($packet, $offset) {
    $bcd = bcd2string(array_slice($packet,$offset,7));
    $year = substr($bcd,0,4);
    $month = substr($bcd,4,2);
    $day = substr($bcd,6,2);
    $hour = substr($bcd,8,2);
    $minute = substr($bcd,10,2);
    $second = substr($bcd,12,2);

    return sprintf('%s-%s-%s %s:%s:%s',$year,$month,$day,$hour,$minute,$second);
}

function bcd2string ($bytes) {
  return join('',array_map(fn($v) => sprintf('%02x',$v),$bytes));
}

?>