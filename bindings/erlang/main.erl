-module(main).

-export([uhppoted/0, uhppoted/1]).

-define(ANY, "0.0.0.0:0").
-define(BROADCAST, { {255,255,255,255}, 60000 }).
-define(LISTEN, "0.0.0.0:60001").

-record(config, { bind, broadcast, listen, debug }).

uhppoted() ->
    io:fwrite("uhppoted-codegen: Erlang sample application~n~n"),
    usage().

uhppoted(Args) ->
    io:fwrite("uhppoted-codegen: Erlang sample application~n~n"),
    { Config, Commands } = parse(Args),
    exec(Commands, Config).

exec(["all"], Config) ->
    exec([C || {C,_} <- commands:commands(), C /= "listen"], Config);

exec(Commands, Config) ->
    lists:foreach(fun(C) -> execute(C, commands:find(C), Config) end, Commands).

execute(Cmd, false, _) ->
    io:fwrite("~n"),
    io:fwrite("   *** ERROR: invalid command ~s~n",[Cmd]),
    io:fwrite("~n"),
    erlang:error({invalid_command, Cmd});

execute(_, C, Config) ->
    commands:exec(C, Config).

parse(Args) ->
    { #config{ 
         bind = ?ANY,
         broadcast = ?BROADCAST,
         listen = ?LISTEN,
         debug = true
      }, 
      Args 
    }.

usage() ->
    io:fwrite("  Usage: go run main uhppoted [--debug] [--bind <address>] [--broadcast <address>] [commands]~n"),
    io:fwrite("~n"),
    io:fwrite("    Options:~n"),
    io:fwrite("    --debug                Displays sent and received UDP packets~n"),
    io:fwrite("    --bind <address>       IPv4 address to which to bind. Defaults to 0.0.0.0~n"),
    io:fwrite("    --broadcast <address>  IPv4 address to which for UDP broadcast. Defaults to 255.255.255.255:60000~n"),
    io:fwrite("    --listen <address>     IPv4 address on which to listen for controller events. Defaults to 0.0.0.0:60001~n"),
    io:fwrite("~n"),
    io:fwrite("    Commands:~n"),

    lists:foreach(fun({C,_}) -> io:fwrite("      ~s\~n", [C]) end, commands:commands()),

    io:fwrite("~n").

