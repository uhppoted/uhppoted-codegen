<?php

// Maybe use select rather
// https://www.php.net/manual/en/function.socket-select.php
// https://stackoverflow.com/questions/389645/set-a-timeout-on-socket-read

function udp_broadcast($uhppote, $request) {
    $packet = pack('C*', ...$request);
    $bind = IPv4($uhppote->bind);
    $dest = IPv4($uhppote->broadcast);

    if ($socket = socket_create(AF_INET, SOCK_DGRAM, SOL_UDP)) {
        socket_set_option($socket, SOL_SOCKET, SO_BROADCAST, 1);
        socket_set_option($socket, SOL_SOCKET, SO_SNDTIMEO, array("sec"=>5, "usec"=>0));
        socket_set_option($socket, SOL_SOCKET, SO_RCVTIMEO, array("sec"=>2, "usec"=>0));

        if (!socket_bind($socket, $bind['address'], $bind['port'])) {
            $errorcode = socket_last_error();
            $errormsg = socket_strerror($errorcode);
        
            throw new Exception("failed to bind UDP socket to $uhppote->bind ($errormsg)");                    
        }

        socket_sendto($socket, $packet, 64, 0, $dest['address'], $dest['port']);

        // FIXME loop until total timeout
        //       https://www.php.net/manual/en/function.socket-select.php
        $replies = array();

        do {
            $address = '';
            $port = 0;
            $N = socket_recvfrom($socket, $buffer, 64, 0, $address, $port);

            $reply = unpack("C*", $buffer);

            if ($N == 64) {
                array_push($replies, array_values($reply));                
            }
        } while ($N !== false);
  
        return $replies;

    } else {
        $errorcode = socket_last_error();
        $errormsg = socket_strerror($errorcode);
        
        throw new Exception("failed to create UDP socket ($errormsg)");                    
    }
}

function udp_listen($uhppote, $handlerfn) {
    $bind = IPv4($uhppote->listen);

    if ($socket = socket_create(AF_INET, SOCK_DGRAM, SOL_UDP)) {        
        if (!socket_bind($socket, $bind['address'], $bind['port'])) {
            $errorcode = socket_last_error();
            $errormsg = socket_strerror($errorcode);
        
            throw new Exception("failed to bind to 0.0.0.0:60001 ($errormsg)");
        }

        do {
            $address = '';
            $port = 0;
            $N = socket_recvfrom($socket, $buffer, 64, 0, $address, $port);

            $packet = unpack("C*", $buffer);

            if ($N == 64) {
                $handlerfn(array_values($packet));
            }

        } while ($N !== false);
    } else {
        $errorcode = socket_last_error();
        $errormsg = socket_strerror($errorcode);
        
        throw new Exception("failed to create UDP socket ($errormsg)");                    
    }
}

function IPv4($address) {
    $addr = explode(':',$address);
    $port = 0;

    if (isset($addr[1])) {
        $port = $addr[1];
    }

    return array(
        'address' => $addr[0],
        'port' => $port
    );
}

?>
