<?php

include "uhppote/uhppote.php";

function execute($fn) {
    try {
        pprint($fn());
    } catch (Exception $e) {
        echo "\n   *** ERROR:  ",  $e->getMessage(), "\n\n";
    }
}

function pprint($result) {
    var_dump($result);
}

function get_all_controllers() {
    return uhppote_get_all_controllers();
}

function listen() {
    uhppote_listen(function ($event) {
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