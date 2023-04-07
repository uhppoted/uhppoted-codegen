<?php

include "uhppote/uhppote.php";

function execute($fn, $options) {
    $uhppote = new UHPPOTE(
        $options['bind'],
        $options['broadcast'],
        $options['listen'],
        $options['debug']);

    try {
        pprint($fn($uhppote));
    } catch (Exception $e) {
        echo "\n   *** ERROR:  ",  $e->getMessage(), "\n\n";
    }
}

function pprint($result) {
    var_dump($result);
}

function get_all_controllers($u) {
    return uhppote_get_all_controllers($u);
}

function listen($u) {
    uhppote_listen($u, function ($event) {
        pprint($event);
    });
}

function commands() {
    return  [
        'get-all-controllers' => 'get_all_controllers',
        'listen' => 'listen'
    ];
}
?>