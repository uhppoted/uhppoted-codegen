<?php

include "encode.php";
include "decode.php";
include "udp.php";

function uhppote_get_all_controllers() {
    $request = get_controller_request(0);
    $replies = broadcast($request);

    $list = array();
    foreach ($replies as $reply) {
        $response = get_controller_response($reply);
        array_push($list, $response);
    }

    return $list;
}

?>