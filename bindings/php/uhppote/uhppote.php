<?php

namespace uhppote;

include "encode.php";
include "decode.php";
include "udp.php";

class UHPPOTE
{
    public $bind;
    public $broadcast;
    public $listen;
    public $timeout;
    public $debug;

    public function __construct(
        string $bind='0.0.0.0:0',
        string $broadcast='255.255.255.255:60000',
        string $listen='0.0.0.0:60001',
        int    $timeout=5,
        bool   $debug=false
    ) {
        $this->bind = $bind;
        $this->broadcast = $broadcast;
        $this->listen = $listen;
        $this->timeout = $timeout;
        $this->debug = $debug;
    }
}

function get_all_controllers($uhppote)
{
    $request = get_controller_request(0);
    $replies = udp\broadcast($uhppote, $request);

    $list = array();
    foreach ($replies as $reply) {
        $response = get_controller_response($reply);
        array_push($list, $response);
    }

    return $list;
}

function get_controller($uhppote, $controller)
{
    $request = get_controller_request($controller);
    $reply = udp\send($uhppote, $request);
    $response = get_controller_response($reply);

    return $response;
}


function set_ip($uhppote, $controller, $address, $netmask, $gateway)
{
    $request = set_ip_request($controller, $address, $netmask, $gateway);
    $reply = udp\send($uhppote, $request);

    return array('' => 'ok');
}

function listen($uhppote, $handlerfn)
{
    $fn = function ($packet) use ($handlerfn) {
        try {
            $event = event($packet);

            $handlerfn($event);
        } catch (Exception $e) {
            echo "\n   *** WARN:   ",  $e->getMessage(), "\n\n";
        }
    };

    udp\listen($uhppote, $fn);
}
