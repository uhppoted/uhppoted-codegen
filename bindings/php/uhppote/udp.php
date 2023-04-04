<?php

// Maybe use select rather
// https://www.php.net/manual/en/function.socket-select.php
// https://stackoverflow.com/questions/389645/set-a-timeout-on-socket-read

function broadcast($request) {
    $packet = pack('C*', ...$request);

    if ($socket = socket_create(AF_INET, SOCK_DGRAM, SOL_UDP)) {
        socket_set_option($socket, SOL_SOCKET, SO_BROADCAST, 1);
        socket_set_option($socket, SOL_SOCKET, SO_SNDTIMEO, array("sec"=>5, "usec"=>0));
        socket_set_option($socket, SOL_SOCKET, SO_RCVTIMEO, array("sec"=>2, "usec"=>0));

        socket_sendto($socket, $packet, 64, 0, '192.168.1.255', 60000);

        // FIXME loop until total timeout
        //       https://www.php.net/manual/en/function.socket-select.php
        $replies = array();

        do {
            $address = '';
            $port = 0;
            $N = socket_recvfrom($socket, $reply, 64, 0, $address, $port);

            $response = unpack("C*", $reply);

            if ($N == 64) {
                array_push($replies,$response);                
            }
        } while ($N !== false);
  
        return $replies;

    } else {
        throw new Exception('Error creating UDP socket');
    }
}


?>
