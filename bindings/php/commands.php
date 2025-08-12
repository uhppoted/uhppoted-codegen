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
define('LISTENER_INTERVAL', 15);

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
    $controller = resolve(CONTROLLER);

    return uhppote\get_controller($u, $controller);
}

function set_ipv4($u, $args)
{
    $controller = resolve(CONTROLLER);
    $address = ADDRESS;
    $netmask = NETMASK;
    $gateway = GATEWAY;

    uhppote\set_ipv4($u, $controller, $address, $netmask, $gateway);

    return (object) array('set' => 'ok');
}

function get_time($u, $args)
{
    $controller = resolve(CONTROLLER);

    return uhppote\get_time($u, $controller);
}

function set_time($u, $args)
{
    $controller = resolve(CONTROLLER);
    $now = new DateTimeImmutable('now', new DateTimeZone('PDT'));

    return uhppote\set_time($u, $controller, $now);
}

function get_listener($u, $args)
{
    $controller = resolve(CONTROLLER);
    return uhppote\get_listener($u, $controller);
}

function set_listener($u, $args)
{
    $controller = resolve(CONTROLLER);
    list($address, $port) = explode(':', LISTENER);
    $interval = LISTENER_INTERVAL;

    return uhppote\set_listener($u, $controller, $address, (int)$port, (int) $interval);
}

function get_door($u, $args)
{
    $controller = resolve(CONTROLLER);
    $door = DOOR;

    return uhppote\get_door($u, $controller, $door);
}

function set_door_control($u, $args)
{
    $controller = resolve(CONTROLLER);
    $door = DOOR;
    $mode = MODE;
    $delay = DELAY;

    return uhppote\set_door_control($u, $controller, $door, $mode, $delay);
}

function get_status($u, $args)
{
    $controller = resolve(CONTROLLER);

    return uhppote\get_status($u, $controller);
}

function open_door($u, $args)
{
    $controller = resolve(CONTROLLER);
    $door = DOOR;

    return uhppote\open_door($u, $controller, $door);
}

function get_cards($u, $args)
{
    $controller = resolve(CONTROLLER);

    return uhppote\get_cards($u, $controller);
}

function get_card($u, $args)
{
    $controller = resolve(CONTROLLER);
    $card = CARD;

    return uhppote\get_card($u, $controller, $card);
}

function get_card_by_index($u, $args)
{
    $controller = resolve(CONTROLLER);
    $index = CARD_INDEX;

    return uhppote\get_card_by_index($u, $controller, $index);
}

function put_card($u, $args)
{
    $controller = resolve(CONTROLLER);
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
    $controller = resolve(CONTROLLER);
    $card = CARD;

    return uhppote\delete_card($u, $controller, $card);
}

function delete_all_cards($u, $args)
{
    $controller = resolve(CONTROLLER);

    return uhppote\delete_all_cards($u, $controller);
}

function get_event($u, $args)
{
    $controller = resolve(CONTROLLER);
    $index = EVENT_INDEX;

    return uhppote\get_event($u, $controller, $index);
}

function get_event_index($u, $args)
{
    $controller = resolve(CONTROLLER);

    return uhppote\get_event_index($u, $controller);
}

function set_event_index($u, $args)
{
    $controller = resolve(CONTROLLER);
    $index = EVENT_INDEX;

    return uhppote\set_event_index($u, $controller, $index);
}


function record_special_events($u, $args)
{
    $controller = resolve(CONTROLLER);
    $enabled = true;

    return uhppote\record_special_events($u, $controller, $enabled);
}

function get_time_profile($u, $args)
{
    $controller = resolve(CONTROLLER);
    $profile_id = TIME_PROFILE_ID;

    return uhppote\get_time_profile($u, $controller, $profile_id);
}

function set_time_profile($u, $args)
{
    $controller = resolve(CONTROLLER);
    $profile_id = TIME_PROFILE_ID;
    $start = new DateTimeImmutable('2023-01 -01', new DateTimeZone('PDT'));
    $end = new DateTimeImmutable('2023-12-31', new DateTimeZone('PDT'));
    $monday = true;
    $tuesday = true;
    $wednesday = false;
    $thursday = true;
    $friday = false;
    $saturday = false;
    $sunday = true;
    $segment1_start = new DateTimeImmutable('08:30', new DateTimeZone('PDT'));
    $segment1_end = new DateTimeImmutable('11:45', new DateTimeZone('PDT'));
    $segment2_start = new DateTimeImmutable('13:15', new DateTimeZone('PDT'));
    $segment2_end = new DateTimeImmutable('16:30', new DateTimeZone('PDT'));
    $segment3_start = new DateTimeImmutable('19:30', new DateTimeZone('PDT'));
    $segment3_end = new DateTimeImmutable('20:55', new DateTimeZone('PDT'));
    $linked_profile_id = 30;


    return uhppote\set_time_profile(
        $u,
        $controller,
        $profile_id,
        $start,
        $end,
        $monday,
        $tuesday,
        $wednesday,
        $thursday,
        $friday,
        $saturday,
        $sunday,
        $segment1_start,
        $segment1_end,
        $segment2_start,
        $segment2_end,
        $segment3_start,
        $segment3_end,
        $linked_profile_id
    );
}

function delete_all_time_profiles($u, $args)
{
    $controller = resolve(CONTROLLER);

    return uhppote\delete_all_time_profiles($u, $controller);
}

function add_task($u, $args)
{
    $controller = resolve(CONTROLLER);
    $start_date = new DateTimeImmutable('2023-01 -01', new DateTimeZone('PDT'));
    $end_date = new DateTimeImmutable('2023-12-31', new DateTimeZone('PDT'));
    $monday = true;
    $tuesday = true;
    $wednesday = false;
    $thursday = true;
    $friday = false;
    $saturday = false;
    $sunday = true;
    $start_time = new DateTimeImmutable('08:30', new DateTimeZone('PDT'));
    $door = DOOR;
    $task_type = 2;
    $more_cards = 0;

    return uhppote\add_task(
        $u,
        $controller,
        $start_date,
        $end_date,
        $monday,
        $tuesday,
        $wednesday,
        $thursday,
        $friday,
        $saturday,
        $sunday,
        $start_time,
        $door,
        $task_type,
        $more_cards
    );
}

function refresh_tasklist($u, $args)
{
    $controller = resolve(CONTROLLER);

    return uhppote\refresh_tasklist($u, $controller);
}

function clear_tasklist($u, $args)
{
    $controller = resolve(CONTROLLER);

    return uhppote\clear_tasklist($u, $controller);
}

function set_pc_control($u, $args)
{
    $controller = resolve(CONTROLLER);
    $enabled = true;

    return uhppote\set_pc_control($u, $controller, $enabled);
}

function set_interlock($u, $args)
{
    $controller = resolve(CONTROLLER);
    $interlock = 3;

    return uhppote\set_interlock($u, $controller, $interlock);
}

function activate_keypads($u, $args)
{
    $controller = resolve(CONTROLLER);
    $reader1 = true;
    $reader2 = true;
    $reader3 = false;
    $reader4 = true;

    return uhppote\activate_keypads($u, $controller, $reader1, $reader2, $reader3, $reader4);
}

function set_door_passcodes($u, $args)
{
    $controller = resolve(CONTROLLER);
    $door = DOOR;
    $passcode1 = 12345;
    $passcode2 = 0;
    $passcode3 = 999999;
    $passcode4 = 54321;

    return uhppote\set_door_passcodes($u, $controller, $door, $passcode1, $passcode2, $passcode3, $passcode4);
}

function get_antipassback($u, $args)
{
    $controller = resolve(CONTROLLER);

    return uhppote\get_antipassback($u, $controller);
}

function set_antipassback($u, $args)
{
    $controller = resolve(CONTROLLER);
    $antipassback = 2;

    return uhppote\set_antipassback($u, $controller, $antipassback);
}

function restore_default_parameters($u, $args)
{
    $controller = resolve(CONTROLLER);

    return uhppote\restore_default_parameters($u, $controller);
}

function listen($u, $args)
{
    uhppote\listen($u, function ($event) {
        pprint('event', $event);
    });
}

function resolve($controller)
{ 
    $list = controllers();

    if (isset($list[$controller])) {
        return $list[$controller];
    }

    return new \uhppote\Controller($controller, '', 'udp');
}

function commands()
{
    return  [
        'get-all-controllers' => 'get_all_controllers',
        'get-controller' => 'get_controller',
        'set-IPv4' => 'set_ipv4',
        'get-time' => 'get_time',
        'set-time' => 'set_time',
        'get-listener' => 'get_listener',
        'set-listener' => 'set_listener',
        'get-door' => 'get_door',
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
        'get-time-profile' => 'get_time_profile',
        'set-time-profile' => 'set_time_profile',
        'delete-all-time-profiles' => 'delete_all_time_profiles',
        'add-task' => 'add_task',
        'refresh-tasklist' => 'refresh_tasklist',
        'clear-tasklist' => 'clear_tasklist',
        'set-pc-control' => 'set_pc_control',
        'set-interlock' => 'set_interlock',
        'activate-keypads' => 'activate_keypads',
        'set-door-passcodes' => 'set_door_passcodes',
        'get-antipassback' => 'get_antipassback',
        'set-antipassback' => 'set_antipassback',
        'restore-default-parameters' => 'restore_default_parameters',
        'listen' => 'listen'
    ];
}

function controllers()
{
    return array(
        405419896 => new \uhppote\Controller(405419896, '192.168.1.100:60000', 'tcp'),
        303986753 => new \uhppote\Controller(303986753, '192.168.1.100:60000', 'udp'),
    );
}


