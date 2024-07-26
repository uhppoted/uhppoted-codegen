<?php

namespace uhppote;

include "encode.php";
include "decode.php";
include "ut0311.php";

class UHPPOTE
{
    public $bind;
    public $broadcast;
    public $listen;
    public $timeout;
    public $debug;

    public function __construct(
        string $bind='0.0.0.0:0',
        string $broadcast='255.255.255.255:60000',
        string $listen='0.0.0.0:60001',
        int    $timeout=5,
        bool   $debug=false
    ) {
        $this->bind = $bind;
        $this->broadcast = $broadcast;
        $this->listen = $listen;
        $this->timeout = $timeout;
        $this->debug = $debug;
    }
}

class Controller {
    public $controller;
    public $address;
    public $transport;

    public function __construct(int $controller, string $address = '', string $transport = '') {
        $this->controller = $controller;
        $this->address = $address;
        $this->transport = $transport;
    }
}

function get_all_controllers($uhppote)
{
    $request = get_controller_request(0);
    $replies = ut0311\broadcast($uhppote, $request);

    $list = array();
    foreach ($replies as $reply) {
        $response = get_controller_response($reply);
        array_push($list, $response);
    }

    return $list;
}

function listen($uhppote, $handlerfn)
{
    $fn = function ($packet) use ($handlerfn) {
        try {
            $event = event($packet);

            $handlerfn($event);
        } catch (Exception $e) {
            echo "\n   *** WARN:   ",  $e->getMessage(), "\n\n";
        }
    };

    ut0311\listen($uhppote, $fn);
}

{{range $ix,$fn := .model.functions}}
{{- template "function" . -}}
{{end}}

{{define "function"}}
function {{snakeCase .name}}($uhppote, {{template "args" .args}})
{
    $c = resolve($controller);
    $request = {{snakeCase .request.name}}($c->controller, {{template "params" slice .args 1}});
    $reply = ut0311\send($uhppote, $c, $request);
    {{if .response}}$response = {{snakeCase .response.name}}($reply);

    return $response;
    {{else}}
    return array('' => 'ok');
    {{end}}
}
{{end}}

function resolve($controller)
{ 
    if (gettype($controller) === 'integer') {
        return new Controller($controller, '', 'udp');
    }

    if (gettype($controller) === 'object' and get_class($controller) === 'uhppote\Controller') {
        return $controller;
    }

    return new Controller(0, '', 'udp');
}
