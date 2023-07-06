-module(main).

-export([uhppoted/0, uhppoted/1]).

uhppoted() ->
    io:fwrite("uhppoted-codegen: Erlang sample application~n"),
    usage().

uhppoted(Args) ->
    io:fwrite("uhppoted-codegen: Erlang sample application~n"),
    io:format("Args:~p~n", [Args]).

usage() ->
    io:fwrite("  Usage: go run main.go [--debug] [--bind <address>] [--broadcast <address>] [commands]~n"),
    io:fwrite("~n"),
    io:fwrite("    Options:~n"),
    io:fwrite("    --debug                Displays sent and received UDP packets~n"),
    io:fwrite("    --bind <address>       IPv4 address to which to bind. Defaults to 0.0.0.0~n"),
    io:fwrite("    --broadcast <address>  IPv4 address to which for UDP broadcast. Defaults to 255.255.255.255:60000~n"),
    io:fwrite("    --listen <address>     IPv4 address on which to listen for controller events. Defaults to 0.0.0.0:60001~n"),
    io:fwrite("~n"),
    io:fwrite("    Commands:~n"),

    lists:foreach(fun({Cmd}) -> io:fwrite("      ~s\~n", [Cmd]) end, commands:commands()),

    io:fwrite("~n").

