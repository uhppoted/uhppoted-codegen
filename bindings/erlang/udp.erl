-module(udp).

-export([ broadcast/2, send/2, listen/2 ]).

-record(config, { bind, broadcast, listen, debug }).

-define(READ_TIMEOUT, 2500).
-define(LOG_TAG, "udp").

broadcast (Config, Request) ->
    dump(Request, Config#config.debug),

    Addr = Config#config.broadcast,
    Opts = [ {broadcast, true}, {send_timeout, 1000} ],

    case gen_udp:open(0, [inet, binary, {active,true}]) of
        {ok, Socket} -> 
            Result = broadcast(Socket, Addr, Opts, Request,Config#config.debug),
            gen_udp:close(Socket),
            Result;

        {error, Reason} ->
            {error, Reason}
    end.

broadcast(Socket, DestAddr, Opts, Request, Debug) ->
    case sendto(Socket,DestAddr,Opts,Request) of 
        ok ->
            erlang:send_after(?READ_TIMEOUT, self(), timeout),
            {ok, Received} = read_all(),
            dump_all(Received, Debug),
            {ok, Received};

        {error, Reason} ->
            {error, Reason}
    end.    


send (Config, Request) ->
    dump(Request, Config#config.debug),

    Addr = Config#config.broadcast,
    Opts = [ {broadcast, true}, {send_timeout, 1000} ],

    case gen_udp:open(0, [inet, binary, {active,true}]) of
        {ok, Socket} -> 
            Result = send(Socket, Addr, Opts, Request,Config#config.debug),
            gen_udp:close(Socket),
            Result;

        {error, Reason} ->
            {error, Reason}
    end.

send(Socket, DestAddr, Opts, Request, Debug) ->
    case sendto(Socket,DestAddr,Opts,Request) of 
        ok ->
            erlang:send_after(?READ_TIMEOUT, self(), timeout),
            
            case read() of
                {ok, Received} -> 
                    dump(Received, Debug),
                    {ok, Received};

                {error, Reason} ->
                    {error, Reason}
            end;

        {error, Reason} ->
            {error, Reason}
    end.    


listen (Config, Handler) ->
    { Addr,Port} = Config#config.listen,

    case gen_udp:open(Port, [inet, binary, {active,false}, {ip, Addr} ]) of
        {ok, Socket} -> 
            spawn(fun() -> 
                listen(Socket, Handler, Config#config.debug),
                gen_udp:close(Socket),
                Handler ! closed
                end),
            { ok, fun() -> gen_udp:close(Socket) end };

        {error, Reason} ->
            {error, Reason}
    end.

listen (Socket, Handler, Debug) ->
    case gen_udp:recv(Socket,64) of
        {ok, { _, _, Packet}} ->
            dump(Packet, Debug),
            Handler ! {ok, Packet},
            listen(Socket, Handler, Debug);

        {error, closed} ->
            log:infof(?LOG_TAG,closed),
            {error, closed};

        {error, Reason} ->
            log:infof(?LOG_TAG,io_lib:format("error ~p",[Reason])),
            {error, Reason}
    end.


sendto (Socket, DestAddr, [H | Opts], Request) ->
    case inet:setopts(Socket, [H]) of
        ok -> 
            sendto(Socket, DestAddr, Opts, Request);

        {error, Reason} -> 
            {error, Reason}
    end;


sendto (Socket, DestAddr, [], Request) ->
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


read() ->
    receive
        { udp,_,_,_,Packet } -> 
          {ok, Packet };

        timeout ->
          { error, timeout }
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

