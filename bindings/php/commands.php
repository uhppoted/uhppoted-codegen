<?php

include "uhppote/uhppote.php";

function get_all_controllers() {
    return uhppote_get_all_controllers();
}

function commands() {
    return  [
        'get-all-controllers' => 'get_all_controllers'
    ];
}
?>