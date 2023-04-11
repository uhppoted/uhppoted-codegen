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

        dump($request,$uhppote->debug);

        socket_sendto($socket, $packet, 64, 0, $dest['address'], $dest['port']);

        // FIXME loop until total timeout
        //       https://www.php.net/manual/en/function.socket-select.php
        $replies = array();

        do {
            $address = '';
            $port = 0;
            $N = socket_recvfrom($socket, $buffer, 64, 0, $address, $port);

            if ($N == 64) {
                $reply = unpack("C*", $buffer);
                dump(array_values($reply),$uhppote->debug);
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

function udp_send($uhppote, $request) {
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

        dump($request,$uhppote->debug);

        socket_sendto($socket, $packet, 64, 0, $dest['address'], $dest['port']);

        // set-ip doesn't return a reply
        if ($request[1] ==  0x96) {
            return [];
        }

        // FIXME loop timeout 
        do {
            $address = '';
            $port = 0;
            $N = socket_recvfrom($socket, $buffer, 64, 0, $address, $port);

            if ($N == 64) {
                $reply = unpack("C*", $buffer);
                dump(array_values($reply),$uhppote->debug);
                return array_values($reply);
            }
        } while ($N !== false);
  
        throw new Exception('no response from controller');                    

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

            if ($N == 64) {
                $packet = unpack("C*", $buffer);
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

function dump($packet, $debug) {
    if ($debug) {
        $offset = 0;

        for ($i = 0; $i < 4; $i++) {
            $p = sprintf('%02x %02x %02x %02x %02x %02x %02x %02x', 
                         $packet[$offset+0],
                         $packet[$offset+1],
                         $packet[$offset+2],
                         $packet[$offset+3],
                         $packet[$offset+4],
                         $packet[$offset+5],
                         $packet[$offset+6],
                         $packet[$offset+7]);

            $q = sprintf('%02x %02x %02x %02x %02x %02x %02x %02x', 
                         $packet[$offset+8],
                         $packet[$offset+9],
                         $packet[$offset+10],
                         $packet[$offset+11],
                         $packet[$offset+12],
                         $packet[$offset+13],
                         $packet[$offset+14],
                         $packet[$offset+15]);

            printf ("   %08x  %s  %s\n", $offset,$p,$q);

            $offset += 16;
        }
    
        print("\n");        
    }
}
?>


