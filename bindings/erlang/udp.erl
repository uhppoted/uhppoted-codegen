-module(udp).

-export([ broadcast/2 ]).

-record(config, { bind, broadcast, listen, debug }).

-define(READ_TIMEOUT, 2500).

broadcast (Config, Request) ->
    dump(Request, Config#config.debug),

    Addr = Config#config.broadcast,
    Opts = [ {broadcast, true}, {send_timeout, 1000} ],

    case gen_udp:open(0, [binary, {active,true}]) of
        {ok, Socket} -> 
            broadcast(Socket, Addr, Opts, Request,Config#config.debug);

        {error, Reason} ->
            {error, Reason}
    end.

broadcast(Socket, DestAddr, Opts, Request, Debug) ->
    case send(Socket,DestAddr,Opts,Request) of 
        ok ->
            erlang:send_after(?READ_TIMEOUT, self(), timeout),
            {ok, Received} = read_all(),
            gen_udp:close(Socket),
            dump_all(Received, Debug),
            {ok, Received};

        {error, Reason} ->
            gen_udp:close(Socket),
            {error, Reason}
    end.    


send (Socket, DestAddr, [H | Opts], Request) ->
    case inet:setopts(Socket, [H]) of
        ok -> 
            send(Socket, DestAddr, Opts, Request);

        {error, Reason} -> 
            {error, Reason}
    end;


send (Socket, DestAddr, [], Request) ->
    gen_udp:send(Socket, DestAddr, Request).


read_all () ->
    read_all([]).

read_all (Received) ->
    receive
        { udp,_,_,_,Packet } -> 
          read_all([ Packet | Received]);

        timeout ->
          { ok, Received }
    end.


dump_all([], true) ->
    ok;

dump_all([Packet | T], true) ->
    bin2hex(Packet,0),
    dump_all(T, true);

dump_all(_, _) ->
    ok.


dump(Packet, true) ->
    bin2hex(Packet,0);

dump(_, _) ->
    ok.


bin2hex(<<P:8/binary, Q:8/binary, Rest/binary>>, Offset) ->
    <<P1:8, P2:8, P3:8, P4:8, P5:8, P6:8, P7:8, P8:8>> = P,
    <<Q1:8, Q2:8, Q3:8, Q4:8, Q5:8, Q6:8, Q7:8, Q8:8>> = Q,

    F = "~2.16.0B ~2.16.0B ~2.16.0B ~2.16.0B ~2.16.0B ~2.16.0B ~2.16.0B ~2.16.0B",

    io:fwrite("   ~8.16.0B  ~s  ~s~n", [ 
        Offset, 
        io_lib:format(F,[ P1, P2, P3, P4, P5, P6, P7, P8 ]), 
        io_lib:format(F,[ Q1, Q2, Q3, Q4, Q5, Q6, Q7, Q8 ])
    ]),

    bin2hex(Rest,Offset+16);

bin2hex(_, _) ->
    io:fwrite("~n"),
    ok.

