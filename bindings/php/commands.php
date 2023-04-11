<?php

include "uhppote/uhppote.php";

define('CONTROLLER',      405419896);
define('DOOR',            3);
define('MODE',            2);
define('DELAY',           10);
define('CARD',            10058400);
define('CARD_INDEX',      3);
define('EVENT_INDEX',     37);
define('TIME_PROFILE_ID', 29);

define('ADDRESS',  '192.168.1.100');
define('NETMASK',  '255.255.255.0');
define('GATEWAY',  '192.168.1.1');
define('LISTENER', '192.168.1.100:60001');

function execute($fn, $options, $args) {
    $uhppote = new UHPPOTE(
        $options['bind'],
        $options['broadcast'],
        $options['listen'],
        $options['timeout'],
        $options['debug']);

    try {
        pprint($fn($uhppote, $args));
    } catch (Exception $e) {
        echo "\n   *** ERROR:  ",  $e->getMessage(), "\n\n";
    }
}

function pprint($result) {
    var_dump($result);
}

function get_all_controllers($u, $args) {
    return uhppote_get_all_controllers($u);
}

function get_controller($u, $args) {
    $controller = CONTROLLER;

    return uhppote_get_controller($u, $controller);
}

function set_ip($u, $args) {
    $controller = CONTROLLER;
    $address = ADDRESS;
    $netmask = NETMASK;
    $gateway = GATEWAY;

    return uhppote_set_ip($u, $controller, $address, $netmask, $gateway);
}

function listen($u, $args) {
    uhppote_listen($u, function ($event) {
        pprint($event);
    });
}

function commands() {
    return  [
        'get-all-controllers' => 'get_all_controllers',
        'get-controller' => 'get_controller',
        'set-ip' => 'set_ip',
        'listen' => 'listen'
    ];
}
?>