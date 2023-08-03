-module(main).

-export([uhppoted/0, uhppoted/1]).

-define(ANY, {any, 0}).
-define(BROADCAST, { {255, 255, 255, 255}, 60000}).
-define(LISTEN, {any, 60001}).

-record(opts, {bind, broadcast, listen, debug, args}).
-record(config, {bind, broadcast, listen, debug}).

uhppoted() ->
    io:fwrite("uhppoted-codegen: Erlang sample application~n~n"),

    Opts = getopts(args()),

    uhppoted(
        #config{
            bind = Opts#opts.bind,
            broadcast = Opts#opts.broadcast,
            listen = Opts#opts.listen,
            debug = Opts#opts.debug
        },
        Opts#opts.args
    ).

uhppoted(Args) ->
    io:fwrite("uhppoted-codegen: Erlang sample application~n~n"),

    Opts = getopts(Args),

    uhppoted(
        #config{
            bind = Opts#opts.bind,
            broadcast = Opts#opts.broadcast,
            listen = Opts#opts.listen,
            debug = Opts#opts.debug
        },
        Opts#opts.args
    ).


uhppoted(_, []) ->
    usage();

uhppoted(Config, [Command | Args]) ->
    case Command of
        "all" ->
            all(Config, Args);
        Cmd ->
            execute(Cmd, commands:find(Cmd), Args, Config)
    end.


all(Config, Args) ->
    all(Config, [C || {C, _} <- commands:commands(), C /= "listen"], Args).

all(_, [], _) ->
    ok;

all(Config, [Cmd | T], Args) ->
    execute(Cmd, commands:find(Cmd), Args, Config),
    all(Config, T, Args).


execute(Cmd, false, _, _) ->
    io:fwrite("~n"),
    io:fwrite("   *** ERROR: invalid command ~s~n", [Cmd]),
    io:fwrite("~n"),
    erlang:error({invalid_command, Cmd});

execute(_, Command, Options, Config) ->
    commands:exec(Command, Options, Config).


usage() ->
    io:fwrite(
        "  Usage: go run main uhppoted [--debug] [--bind <address>] [--broadcast <address>] [command]~n"
    ),
    io:fwrite("~n"),
    io:fwrite("    Options:~n"),
    io:fwrite("    --debug                Displays sent and received UDP packets~n"),
    io:fwrite("    --bind <address>       IPv4 address to which to bind. Defaults to 0.0.0.0~n"),
    io:fwrite(
        "    --broadcast <address>  IPv4 address to which for UDP broadcast. Defaults to 255.255.255.255:60000~n"
    ),
    io:fwrite(
        "    --listen <address>     IPv4 address on which to listen for controller events. Defaults to 0.0.0.0:60001~n"
    ),
    io:fwrite("~n"),
    io:fwrite("    Commands:~n"),

    lists:foreach(fun({C, _}) -> io:fwrite("      ~s\~n", [C]) end, commands:commands()),

    io:fwrite("      ~s\~n", ["all"]),
    io:fwrite("~n").

args() ->
    args(init:get_arguments(), []).

args([], Args) ->
    Args;

args([{root, _} | T], Args) ->
    args(T, Args);

args([{bindir, _} | T], Args) ->
    args(T, Args);

args([{progname, _} | T], Args) ->
    args(T, Args);

args([{home, _} | T], Args) ->
    args(T, Args);

args([{noshell, _} | T], Args) ->
    args(T, Args);

args([{K, V} | T], Args) ->
    args(T, lists:append([Args, [K], V])).


getopts(Args) ->
    getopts(Args, #opts{
        bind = ?ANY,
        broadcast = ?BROADCAST,
        listen = ?LISTEN,
        debug = false,
        args = []
    }).

getopts([], Opts) ->
    Opts;

getopts(['-debug' | T], Opts) ->
    getopts(T, Opts#opts{debug = true});

getopts(['-bind', Addr | T], Opts) ->
    getopts(T, Opts#opts{bind = address(Addr)});

getopts(['-broadcast', Addr | T], Opts) ->
    getopts(T, Opts#opts{broadcast = address(Addr)});

getopts(['-listen', Addr | T], Opts) ->
    getopts(T, Opts#opts{listen = address(Addr)});

getopts([H | T], Opts) ->
    Args = lists:append([Opts#opts.args, [H]]),
    getopts(T, Opts#opts{args = Args}).

address(S) ->
    case re:run(S, "([0-9.]+)(?::([0-9]+))?", [{capture, all_but_first, list}]) of
        {match, [Address]} ->
            {ok, Addr} = inet:parse_address(Address),
            {Addr, 0};
        {match, [Address, Port]} ->
            {ok, Addr} = inet:parse_address(Address),
            {Addr, list_to_integer(Port)};
        _ ->
            S
    end.