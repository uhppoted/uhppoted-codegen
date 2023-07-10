-module(commands).

-export([commands/0, find/1, exec/2]).

commands() ->
    [ 
      { "get-all-controllers" },
      { "get-controller" },
      { "listen" }
    ].

find(Cmd) ->
    lists:keyfind(Cmd,1,commands()).

exec(Cmd, Config) ->
    io:fwrite("exec::~p ~p~n", [ Cmd, Config ]).
