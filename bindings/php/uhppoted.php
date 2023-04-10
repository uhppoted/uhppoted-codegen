<?php

include "commands.php";

print "uhppoted-codegen: PHP sample application\n";

$options = array(
    'bind' => '0.0.0.0:0',
    'broadcast' => '255.255.255.255:60000',
    'listen' => '0.0.0.0:60001',
    'debug' => false,
);

$rest_index = null;
$args = getopt('', array( "bind::", "broadcast::", "listen::", "debug"), $rest_index);
$cmd = array_slice($argv, $rest_index);
$rest = array_slice($argv, $rest_index+1);

if (isset($args['bind'])) {
    $options['bind'] = $args['bind'];
}

if (isset($args['broadcast'])) {
    $options['broadcast'] = $args['broadcast'];
}

if (isset($args['listen'])) {
    $options['listen'] = $args['listen'];
}

if (isset($args['debug'])) {
    $options['debug'] = true;
}

if ($cmd) {
    $commands = commands();
    $key = $cmd[0];

    if (isset($commands[$key])) {
        execute($commands[$key], $options, $rest);
    } else if ($key == 'all') {
        all($options, $rest);
    } else {
        usage();
    }
} else {
    usage();
}

function all($options, $args) {
    $commands = commands();

    foreach ($commands as $cmd) {
        if ($cmd != 'listen') {
            execute($cmd, $options, $args);
        }
    }
}

function usage() {
    $commands = commands();

    print "\n";
    print "   Usage: php uhppoted.php [--debug] [--bind <address>] [--broadcast <address>] [--listen <address>] <command> <options>\n";
    print "\n";
    print "   Commands\n";

    foreach ($commands as $key => $cmd) {
        print "      $key\n";
    }

    print "\n";
}

?>