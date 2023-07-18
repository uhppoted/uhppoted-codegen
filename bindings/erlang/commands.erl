-module(commands).

-export([commands/0, find/1, exec/2]).

-define(CONTROLLER, 405419896).

commands() ->
    [ 
      { "get-all-controllers", get_all_controllers },
      { "get-controller", get_controller },
      { "listen", listen }
    ].

find(Cmd) ->
    lists:keyfind(Cmd,1,commands()).

exec({_, Cmd}, Config) ->
    io:format(">> ~p~n", [ execute(Cmd, Config) ]).

execute(get_all_controllers, Config) ->
    uhppoted:get_all_controllers(Config);

execute(get_controller, Config) ->
    Controller = ?CONTROLLER,
    uhppoted:get_controller(Config, Controller);

execute(listen, Config) ->
    uhppoted:listen(Config);

execute(C, _) ->
    erlang:error({not_implemented, C}).
