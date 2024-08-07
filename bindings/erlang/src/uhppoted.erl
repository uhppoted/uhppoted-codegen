-module(uhppoted).

-export([
    get_all_controllers/1, 
    {{- range .model.functions }}{{- template "export" . }},{{end}}
    listen/2
]).

{{- define "export"}}
    {{snakeCase .name}}/{{ add (len .args) 1 -}}
{{end}}

-define(LOG_TAG, "uhppoted").

-include("records.hrl").

get_all_controllers(Config) ->
    Request = encoder:get_controller_request(0),

    case ut0311:broadcast(Config, Request) of
        {ok, Received} ->
            [decoder:get_controller_response(P) || P <- Received];
        {error, Reason} ->
            {error, Reason}
    end.


listen(Config, Handler) ->
    PID = spawn(fun() -> listen(Handler) end),

    case ut0311:listen(Config, PID) of
        {ok, F} ->
            {ok, F};
        {error, Reason} ->
            PID ! cancel,
            {error, Reason}
    end.

listen(Handler) ->
    receive
        {ok, Packet} ->
            case decoder:event(Packet) of
                {ok, Event} ->
                    Handler ! {event, Event}
            end,
            listen(Handler);
        cancel ->
            log:infof(?LOG_TAG, "cancelled"),
            Handler ! closed;
        close ->
            log:infof(?LOG_TAG, "closed"),
            Handler ! closed;
        Any ->
            log:debugf(?LOG_TAG, Any),
            Handler ! closed
    end.


{{- range .model.functions }}
{{ template "function" . -}}
{{end}}

{{define "function"}}
{{snakeCase .name}}(Config, {{template "args" .args}}) ->
    Request = encoder:{{snakeCase .request.name}}(Controller#controller.controller{{template "params" slice .args 1}}),

    case ut0311:send(Config, Controller, Request) of
        {ok, none} ->
            {ok, none};
      {{if .response}}
        {ok, Received} ->
            decoder:{{snakeCase .response.name}}(Received);
      {{- end}}
        {error, Reason} ->
            {error, Reason}
    end.
{{end}}
