<?php

include "uhppote/uhppote.php";

define('CONTROLLER', 405419896);
define('DOOR', 3);
define('MODE', 2);
define('DELAY', 10);
define('CARD', 10058400);
define('CARD_INDEX', 3);
define('EVENT_INDEX', 37);
define('TIME_PROFILE_ID', 29);

define('ADDRESS', '192.168.1.100');
define('NETMASK', '255.255.255.0');
define('GATEWAY', '192.168.1.1');
define('LISTENER', '192.168.1.100:60001');

function execute($cmd, $fn, $options, $args)
{
    $uhppote = new \uhppote\UHPPOTE(
        $options['bind'],
        $options['broadcast'],
        $options['listen'],
        $options['timeout'],
        $options['debug']
    );

    try {
        pprint($cmd, $fn($uhppote, $args));
    } catch (Exception $e) {
        echo "\n   *** ERROR:  ",  $e->getMessage(), "\n\n";
    }
}

function pprint($cmd, $result)
{
    if ($cmd != '') {
        print("   $cmd\n");
    }

    if (is_array($result)) {
        foreach ($result as $response) {
            pprint('', $response);
        }
    } else {
        $width = 0;
        foreach ($result as $key => $value) {
            if (strlen($key) > $width) {
                $width = strlen($key);
            }
        }

        $format = sprintf("      %%-%ds  %%s\n", $width);

        foreach ($result as $key => $value) {
            if (is_bool($value)) {
                printf($format, $key, $value ? 'Y' : 'N');
            } else {
                printf($format, $key, $value);
            }
        }
    }

    print("\n");
}

function get_all_controllers($u, $args)
{
    return uhppote\get_all_controllers($u);
}

function get_controller($u, $args)
{
    $controller = CONTROLLER;

    return uhppote\get_controller($u, $controller);
}

function set_ip($u, $args)
{
    $controller = CONTROLLER;
    $address = ADDRESS;
    $netmask = NETMASK;
    $gateway = GATEWAY;

    uhppote\set_ip($u, $controller, $address, $netmask, $gateway);

    return (object) array('set' => 'ok');
}

function get_time($u, $args)
{
    $controller = CONTROLLER;

    return uhppote\get_time($u, $controller);
}

function set_time($u, $args)
{
    $controller = CONTROLLER;
    $now = new DateTimeImmutable('now', new DateTimeZone('PDT'));

    return uhppote\set_time($u, $controller, $now);
}

function get_listener($u, $args)
{
    $controller = CONTROLLER;

    return uhppote\get_listener($u, $controller);
}

function set_listener($u, $args)
{
    $controller = CONTROLLER;
    list($address, $port) = explode(':', LISTENER);

    return uhppote\set_listener($u, $controller, $address, (int)$port);
}

function get_door_control($u, $args)
{
    $controller = CONTROLLER;
    $door = DOOR;

    return uhppote\get_door_control($u, $controller, $door);
}

function set_door_control($u, $args)
{
    $controller = CONTROLLER;
    $door = DOOR;
    $mode = MODE;
    $delay = DELAY;

    return uhppote\set_door_control($u, $controller, $door, $mode, $delay);
}

function get_status($u, $args)
{
    $controller = CONTROLLER;

    return uhppote\get_status($u, $controller);
}

function open_door($u, $args)
{
    $controller = CONTROLLER;
    $door = DOOR;
 
    return uhppote\open_door($u, $controller, $door);
}

function get_cards($u, $args)
{
    $controller = CONTROLLER;
 
    return uhppote\get_cards($u, $controller);
}

function get_card($u, $args)
{
    $controller = CONTROLLER;
    $card = CARD;
 
    return uhppote\get_card($u, $controller, $card);
}

function get_card_by_index($u, $args)
{
    $controller = CONTROLLER;
    $index = CARD_INDEX;
 
    return uhppote\get_card_by_index($u, $controller, $index);
}

function put_card($u, $args)
{
    $controller = CONTROLLER;
    $card = CARD;
    $start = new DateTimeImmutable('2023-01 -01', new DateTimeZone('PDT'));
    $end = new DateTimeImmutable('2023-12-31', new DateTimeZone('PDT'));
    $door1 = 0;
    $door2 = 1;
    $door3 = 29;
    $door4 = 0;
    $pin = 7531;

    return uhppote\put_card($u, $controller, $card, $start, $end, $door1, $door2, $door3, $door4, $pin);
}

function delete_card($u, $args)
{
    $controller = CONTROLLER;
    $card = CARD;

    return uhppote\delete_card($u, $controller, $card);
}

function delete_all_cards($u, $args)
{
    $controller = CONTROLLER;

    return uhppote\delete_all_cards($u, $controller);
}

function get_event($u, $args)
{
    $controller = CONTROLLER;
    $index = EVENT_INDEX;

    return uhppote\get_event($u, $controller, $index);
}

function get_event_index($u, $args)
{
    $controller = CONTROLLER;

    return uhppote\get_event_index($u, $controller);
}

function set_event_index($u, $args)
{
    $controller = CONTROLLER;
    $index = EVENT_INDEX;

    return uhppote\set_event_index($u, $controller, $index);
}


function record_special_events($u, $args)
{
    $controller = CONTROLLER;
    $enabled = true;

    return uhppote\record_special_events($u, $controller, $enabled);
}

function listen($u, $args)
{
    uhppote\listen($u, function ($event) {
        pprint('event', $event);
    });
}

function commands()
{
    return  [
        'get-all-controllers' => 'get_all_controllers',
        'get-controller' => 'get_controller',
        'set-ip' => 'set_ip',
        'get-time' => 'get_time',
        'set-time' => 'set_time',
        'get-listener' => 'get_listener',
        'set-listener' => 'set_listener',
        'get-door-control' => 'get_door_control',
        'set-door-control' => 'set_door_control',
        'get-status' => 'get_status',
        'open-door' => 'open_door',
        'get-cards' => 'get_cards',
        'get-card' => 'get_card',
        'get-card-by-index' => 'get_card_by_index',
        'put-card' => 'put_card',
        'delete-card' => 'delete_card',
        'delete-all-cards' => 'delete_all_cards',
        'get-event' => 'get_event',
        'get-event-index' => 'get_event_index',
        'set-event-index' => 'set_event_index',
        'record-special-events' => 'record_special_events',
        'listen' => 'listen'
    ];
}
