-module(commands).

-export([commands/0, find/1, exec/1]).

commands() ->
    [ 
      { "get-all-controllers" },
      { "get-controller" },
      { "listen" }
    ].

find(Cmd) ->
    lists:keyfind(Cmd,1,commands()).

exec(Cmd) ->
    io:fwrite("exec::~p~n", [ Cmd ]).
