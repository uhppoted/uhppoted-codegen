<?php

include "encode.php";
include "decode.php";
include "udp.php";

class UHPPOTE {
    public $bind;
    public $broadcast;
    public $listen;
    public $debug;

    public function __construct(string $bind='0.0.0.0:0', string $broadcast='255.255.255.255:60000', string $listen='0.0.0.0:60001', bool $debug=false) {
        $this->bind = $bind;
        $this->broadcast = $broadcast;
        $this->listen = $listen;
        $this->debug = $debug;
    }
}

function uhppote_get_all_controllers($uhppote) {
    $request = get_controller_request(0);
    $replies = udp_broadcast($uhppote, $request);

    $list = array();
    foreach ($replies as $reply) {
        $response = get_controller_response($reply);
        array_push($list, $response);
    }

    return $list;
}

function uhppote_listen($uhppote,$handlerfn) {
    $fn = function($packet) use ($handlerfn) {
        try {
            $event = event($packet);

            $handlerfn($event);
        } catch (Exception $e) {
            echo "\n   *** WARN:   ",  $e->getMessage(), "\n\n";
        }
    };

    udp_listen($uhppote, $fn);
}

?>