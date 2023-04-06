<?php

include "encode.php";
include "decode.php";
include "udp.php";

function uhppote_get_all_controllers() {
    $request = get_controller_request(0);
    $replies = udp_broadcast($request);

    $list = array();
    foreach ($replies as $reply) {
        $response = get_controller_response($reply);
        array_push($list, $response);
    }

    return $list;
}

function uhppote_listen($handlerfn) {
    $fn = function($packet) use ($handlerfn) {
        try {
            print "wooot\n";
            $event = event($packet);

            $handlerfn($event);
        } catch (Exception $e) {
            echo "\n   *** WARN:   ",  $e->getMessage(), "\n\n";
        }
    };

    udp_listen($fn);
}

?>