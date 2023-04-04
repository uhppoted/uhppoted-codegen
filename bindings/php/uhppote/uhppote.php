<?php

include "encode.php";
include "udp.php";

function uhppote_get_all_controllers() {
    $request = get_controller_request(0);
    $replies = broadcast($request);

    // var_dump($replies);

    print("\n\n");
    print(">>>>>>> REPLIES:");
    print(count($replies));
    print("\n\n");

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