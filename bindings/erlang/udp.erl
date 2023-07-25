-module(udp).

-export([ broadcast/2, send/2, listen/2 ]).

-record(config, { bind, broadcast, listen, debug }).

-define(READ_TIMEOUT, 2500).
-define(LOG_TAG, "udp").

broadcast (Config, Request) ->
    dump(Request, Config#config.debug),

    Addr = Config#config.broadcast,

    case gen_udp:open(0, [inet, binary, {active,true}, {broadcast,true} ]) of
        {ok, Socket} -> 
            Result = broadcast(Socket, Addr, Request,Config#config.debug),
            gen_udp:close(Socket),
            Result;

        {error, Reason} ->
            {error, Reason}
    end.

broadcast(Socket, DestAddr, Request, Debug) ->
    case sendto(Socket, DestAddr, Request) of 
        ok ->
            erlang:send_after(?READ_TIMEOUT, self(), timeout),
            read_all(Debug);

        {error, Reason} ->
            {error, Reason}
    end.


send (Config, Request) ->
    dump(Request, Config#config.debug),

    Addr = Config#config.broadcast,

    case gen_udp:open(0, [inet, binary, {active,true}, {broadcast,true}]) of
        {ok, Socket} -> 
            Result = send(Socket, Addr,Request,Config#config.debug),
            gen_udp:close(Socket),
            Result;

        {error, Reason} ->
            {error, Reason}
    end.

% set-ip doesn't return a reply
send(Socket, DestAddr, <<16#17, 16#96,R/binary>>, _Debug) ->
    case sendto(Socket, DestAddr, <<16#17, 16#96,R/binary>>) of 
        ok ->
            erlang:send_after(?READ_TIMEOUT, self(), timeout),
            {ok, none };

        {error, Reason} ->
            {error, Reason}
    end;

send(Socket, DestAddr, Request, Debug) ->
    case sendto(Socket, DestAddr, Request) of 
        ok ->
            erlang:send_after(?READ_TIMEOUT, self(), timeout),
            read(Debug);

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


sendto (Socket, DestAddr,Request) ->
    case inet:setopts(Socket, [{send_timeout, 1000}]) of
        ok -> 
            gen_udp:send(Socket, DestAddr, Request);

        {error, Reason} -> 
            {error, Reason}
    end.


read_all (Debug) ->
    read_all([], Debug).

read_all (Received, Debug) ->
    receive
        { udp,_,_,_,Packet } -> 
          dump(Packet, Debug),
          read_all([ Packet | Received], Debug);

        timeout ->
          { ok, Received }
    end.


read(Debug) ->
    receive
        { udp,_,_,_,Packet } -> 
          dump(Packet, Debug),
          {ok, Packet };

        timeout ->
          { error, timeout }
    end.


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

