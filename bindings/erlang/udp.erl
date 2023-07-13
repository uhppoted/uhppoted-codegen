-module(udp).

-export([ broadcast/2 ]).

-record(config, { bind, broadcast, listen, debug }).

broadcast (Config, Request) ->
    dump(Request, Config#config.debug),

    Addr = Config#config.broadcast,
    Opts = [ {broadcast, true}, {send_timeout, 1000} ],

    case gen_udp:open(0, [binary, {active,false}]) of
        {ok, Socket} -> 
            Result = broadcast(Socket, Addr, Opts, Request),
            dump_all(Result, Config#config.debug),
            gen_udp:close(Socket),
            Result;

        {error, Reason} ->
            {error, Reason}
    end.

broadcast(Socket, DestAddr, Opts, Request) ->
    send(Socket,DestAddr,Opts,Request).
%   Self = self(),
%   _Pid = spawn(fun()-> Self ! send(Socket,DestAddr,Opts,Request) end),
% 
%   receive
%       {ok, X} -> 
%           io:format("wooooot ~p~n",[X]),
%           ok
%   after
%        10000 -> 
%           io:format("eeek~n"),
%           timeout
%   end,
% {ok,[]}.

send (Socket, DestAddr, [H | Opts], Request) ->
    case inet:setopts(Socket, [H]) of
        ok -> 
            send(Socket, DestAddr, Opts, Request);

        {error, Reason} -> 
            {error, Reason}
    end;


send (Socket, DestAddr, [], Request) ->
    case gen_udp:send(Socket, DestAddr, Request) of
        ok -> read_all(Socket, []);

        {error, Reason} -> 
            {error, Reason}
    end.


read_all (Socket, Received) ->
    case gen_udp:recv(Socket,64,2500) of
        {ok, {_,_,Packet}} ->
            read_all(Socket, [Packet | Received]);

        {error, timeout} -> 
            {ok, Received};

        {error, Reason} -> 
            {error, Reason}
    end.

dump_all({ok, [Packet | T]}, true) ->
    bin2hex(Packet,0),
    dump_all({ok, T}, true);

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

