<?php

include "encode.php";
include "decode.php";
include "udp.php";

function uhppote_get_all_controllers() {
    $request = get_controller_request(0);
    $replies = broadcast($request);

    foreach ($replies as $reply) {
        $response = get_controller_response($reply);
        var_dump($response);
    }

    // list := []*GetControllerResponse{}
    // for _, reply := range replies {
    //     if response, err := getControllerResponse(reply); err != nil {
    //         return nil, err
    //     } else if response != nil {
    //         list = append(list, response)
    //     }
    // }

    // return list, nil    
}

?>