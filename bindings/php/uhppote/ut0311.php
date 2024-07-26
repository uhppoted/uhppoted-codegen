<?php

namespace uhppote\ut0311;

function broadcast($uhppote, $request)
{
    // set total timeout
    $timeout = false;

    set_error_handler(function ($errno, $errmsg) use (&$timeout) {
        if (!$timeout) {
            throw new Exception("$errmsg");
        }
    }, E_WARNING);

    pcntl_async_signals(true);
    pcntl_signal(SIGALRM, function ($signal) use (&$timeout) {
        $timeout = true;
    });

    pcntl_alarm($uhppote->timeout);

    // broadcast request
    try {
        $packet = pack('C*', ...$request);
        $bind = IPv4($uhppote->bind);
        $dest = IPv4($uhppote->broadcast);
        $replies = array();

        if ($socket = socket_create(AF_INET, SOCK_DGRAM, SOL_UDP)) {
            socket_set_option($socket, SOL_SOCKET, SO_BROADCAST, 1);
            socket_set_option($socket, SOL_SOCKET, SO_SNDTIMEO, array("sec"=>5, "usec"=>0));
            socket_set_option($socket, SOL_SOCKET, SO_RCVTIMEO, array("sec"=>7, "usec"=>500000));

            if (!socket_bind($socket, $bind['address'], $bind['port'])) {
                $errorcode = socket_last_error();
                $errormsg = socket_strerror($errorcode);

                throw new Exception("failed to bind UDP socket to $uhppote->bind ($errormsg)");
            }

            dump($request, $uhppote->debug);

            socket_sendto($socket, $packet, 64, 0, $dest['address'], $dest['port']);

            do {
                $address = '';
                $port = 0;
                $N = socket_recvfrom($socket, $buffer, 64, 0, $address, $port);

                if ($N == 64) {
                    $reply = unpack("C*", $buffer);
                    dump(array_values($reply), $uhppote->debug);
                    array_push($replies, array_values($reply));
                }
            } while ($N !== false);

            return $replies;

        } else {
            $errorcode = socket_last_error();
            $errormsg = socket_strerror($errorcode);

            throw new Exception("failed to create UDP socket ($errormsg)");
        }
    } finally {
        pcntl_alarm(0);
        restore_error_handler();
    }
}

function send($uhppote, $controller, $request)
{
    if ($controller->address !== '' and $controller->transport === 'tcp') {
        return tcp_send_to($uhppote, $controller->address, $request);        
    }

    if ($controller->address !== '') {
        return udp_send_to($uhppote, $controller->address, $request);        
    }

    return udp_broadcast_to($uhppote, $request);
}

function udp_broadcast_to($uhppote, $request)
{
    // setup async timeout
    $timeout = false;

    set_error_handler(function ($errno, $errmsg) use (&$timeout) {
        if (!$timeout) {
            throw new Exception("$errmsg");
        }
    }, E_WARNING);

    pcntl_async_signals(true);
    pcntl_signal(SIGALRM, function ($signal) use (&$timeout) {
        $timeout = true;
    });

    pcntl_alarm($uhppote->timeout);

    // send request
    $packet = pack('C*', ...$request);
    $bind = IPv4($uhppote->bind);
    $dest = IPv4($uhppote->broadcast);

    try {
        if ($socket = socket_create(AF_INET, SOCK_DGRAM, SOL_UDP)) {
            socket_set_option($socket, SOL_SOCKET, SO_BROADCAST, 1);
            socket_set_option($socket, SOL_SOCKET, SO_SNDTIMEO, array("sec"=>5, "usec"=>0));
            socket_set_option($socket, SOL_SOCKET, SO_RCVTIMEO, array("sec"=>5, "usec"=>0));

            if (!socket_bind($socket, $bind['address'], $bind['port'])) {
                $errorcode = socket_last_error();
                $errormsg = socket_strerror($errorcode);

                throw new Exception("failed to bind UDP socket to $uhppote->bind ($errormsg)");
            }

            dump($request, $uhppote->debug);

            socket_sendto($socket, $packet, 64, 0, $dest['address'], $dest['port']);

            // set-ip doesn't return a reply
            if ($request[1] ==  0x96) {
                return [];
            }

            do {
                $address = '';
                $port = 0;
                $N = socket_recvfrom($socket, $buffer, 64, 0, $address, $port);

                if ($N == 64) {
                    $reply = unpack("C*", $buffer);
                    dump(array_values($reply), $uhppote->debug);
                    return array_values($reply);
                }
            } while ($N !== false);

            throw new Exception('no response from controller');

        } else {
            $errorcode = socket_last_error();
            $errormsg = socket_strerror($errorcode);

            throw new Exception("failed to create UDP socket ($errormsg)");
        }
    } finally {
        pcntl_alarm(0);
        restore_error_handler();
    }
}

function udp_send_to($uhppote, $addr, $request)
{
    // setup async timeout
    $timeout = false;

    set_error_handler(function ($errno, $errmsg) use (&$timeout) {
        if (!$timeout) {
            throw new Exception("$errmsg");
        }
    }, E_WARNING);

    pcntl_async_signals(true);
    pcntl_signal(SIGALRM, function ($signal) use (&$timeout) {
        $timeout = true;
    });

    pcntl_alarm($uhppote->timeout);

    // send request
    $packet = pack('C*', ...$request);
    $bind = IPv4($uhppote->bind);
    $dest = IPv4($addr);

    try {
        if ($socket = socket_create(AF_INET, SOCK_DGRAM, SOL_UDP)) {
            socket_set_option($socket, SOL_SOCKET, SO_SNDTIMEO, array("sec"=>5, "usec"=>0));
            socket_set_option($socket, SOL_SOCKET, SO_RCVTIMEO, array("sec"=>5, "usec"=>0));

            if (!socket_bind($socket, $bind['address'], $bind['port'])) {
                $errorcode = socket_last_error();
                $errormsg = socket_strerror($errorcode);

                throw new Exception("failed to bind UDP socket to $uhppote->bind ($errormsg)");
            }

            if (!socket_connect($socket, $dest['address'], $dest['port'])) {
                $errorcode = socket_last_error();
                $errormsg = socket_strerror($errorcode);

                throw new Exception("error connecting to UDP socket ($errormsg)");
            }

            dump($request, $uhppote->debug);

            socket_send($socket, $packet, 64, 0);

            // set-ip doesn't return a reply
            if ($request[1] ==  0x96) {
                return [];
            }

            do {
                $address = '';
                $port = 0;
                $N = socket_recv($socket, $buffer, 64, 0);

                if ($N == 64) {
                    $reply = unpack("C*", $buffer);
                    dump(array_values($reply), $uhppote->debug);
                    return array_values($reply);
                }
            } while ($N !== false);

            throw new Exception('no response from controller');

        } else {
            $errorcode = socket_last_error();
            $errormsg = socket_strerror($errorcode);

            throw new Exception("failed to create UDP socket ($errormsg)");
        }
    } finally {
        pcntl_alarm(0);
        restore_error_handler();
    }
}

function tcp_send_to($uhppote, $addr, $request)
{
    // setup async timeout
    $timeout = false;

    set_error_handler(function ($errno, $errmsg) use (&$timeout) {
        if (!$timeout) {
            throw new Exception("$errmsg");
        }
    }, E_WARNING);

    pcntl_async_signals(true);
    pcntl_signal(SIGALRM, function ($signal) use (&$timeout) {
        $timeout = true;
    });

    pcntl_alarm($uhppote->timeout);

    // send request
    $packet = pack('C*', ...$request);
    $bind = IPv4($uhppote->bind);
    $dest = IPv4($addr);

    try {
        if ($socket = socket_create(AF_INET, SOCK_STREAM, SOL_TCP)) {
            socket_set_option($socket, SOL_SOCKET, SO_SNDTIMEO, array("sec"=>5, "usec"=>0));
            socket_set_option($socket, SOL_SOCKET, SO_RCVTIMEO, array("sec"=>5, "usec"=>0));

            if (!socket_bind($socket, $bind['address'], $bind['port'])) {
                $errorcode = socket_last_error();
                $errormsg = socket_strerror($errorcode);

                throw new Exception("failed to bind UDP socket to $uhppote->bind ($errormsg)");
            }

            if (!socket_connect($socket, $dest['address'], $dest['port'])) {
                $errorcode = socket_last_error();
                $errormsg = socket_strerror($errorcode);

                throw new Exception("error connecting to UDP socket ($errormsg)");
            }

            dump($request, $uhppote->debug);

            socket_send($socket, $packet, 64, 0);

            // set-ip doesn't return a reply
            if ($request[1] ==  0x96) {
                return [];
            }

            do {
                $address = '';
                $port = 0;
                $N = socket_recv($socket, $buffer, 64, 0);

                if ($N == 64) {
                    $reply = unpack("C*", $buffer);
                    dump(array_values($reply), $uhppote->debug);
                    return array_values($reply);
                }
            } while ($N !== false);

            throw new Exception('no response from controller');

        } else {
            $errorcode = socket_last_error();
            $errormsg = socket_strerror($errorcode);

            throw new Exception("failed to create UDP socket ($errormsg)");
        }
    } finally {
        pcntl_alarm(0);
        restore_error_handler();
    }
}

function listen($uhppote, $handlerfn)
{
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

function IPv4($address)
{
    $addr = explode(':', $address);
    $port = 0;

    if (isset($addr[1])) {
        $port = $addr[1];
    }

    return array(
        'address' => $addr[0],
        'port' => $port
    );
}

function dump($packet, $debug)
{
    if ($debug) {
        $offset = 0;

        for ($i = 0; $i < 4; $i++) {
            $p = sprintf(
                '%02x %02x %02x %02x %02x %02x %02x %02x',
                $packet[$offset+0],
                $packet[$offset+1],
                $packet[$offset+2],
                $packet[$offset+3],
                $packet[$offset+4],
                $packet[$offset+5],
                $packet[$offset+6],
                $packet[$offset+7]
            );

            $q = sprintf(
                '%02x %02x %02x %02x %02x %02x %02x %02x',
                $packet[$offset+8],
                $packet[$offset+9],
                $packet[$offset+10],
                $packet[$offset+11],
                $packet[$offset+12],
                $packet[$offset+13],
                $packet[$offset+14],
                $packet[$offset+15]
            );

            printf("   %08x  %s  %s\n", $offset, $p, $q);

            $offset += 16;
        }

        print("\n");
    }
}
?>


