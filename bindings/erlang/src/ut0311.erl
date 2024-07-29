-module(ut0311).

-export([broadcast/2, send/3, listen/2]).

-record(config, {bind, broadcast, listen, debug}).

-define(CONNECT_TIMEOUT, 5000).
-define(READ_TIMEOUT, 2500).
-define(LOG_TAG, "ut0311").

broadcast(Config, Request) ->
    dump(Request, Config#config.debug),

    Addr = Config#config.broadcast,

    case gen_udp:open(0, [inet, binary, {active, true}, {broadcast, true}]) of
        {ok, Socket} ->
            Result = broadcast(Socket, Addr, Request, Config#config.debug),
            gen_udp:close(Socket),
            Result;
        {error, Reason} ->
            {error, Reason}
    end.

broadcast(Socket, DestAddr, Request, Debug) ->
    case sendto(udp, Socket, DestAddr, Request) of
        ok ->
            erlang:send_after(?READ_TIMEOUT, self(), timeout),
            read_all(Debug);
        {error, Reason} ->
            {error, Reason}
    end.


send(Config, {controller,_,"",_}, Request) ->
    udp_broadcast_to(Config, Request);

send(Config, {controller,_, Address, "tcp"}, Request) ->
    case resolve(Address) of
        {ok, Addr} ->
            tcp_send_to(Config, Addr, Request);
        {error, Reason} ->
            {error, Reason}
    end;

send(Config, {controller,_, Address, _}, Request) ->
    case resolve(Address) of
        {ok, Addr} ->
            udp_send_to(Config, Addr, Request);
        {error, Reason} ->
            {error, Reason}
    end.


udp_broadcast_to(Config, Request) ->
    dump(Request, Config#config.debug),

    Addr = Config#config.broadcast,

    case gen_udp:open(0, [inet, binary, {active, true}, {broadcast, true}]) of
        {ok, Socket} ->
            Result = send(udp,Socket, Addr, Request, Config#config.debug),
            gen_udp:close(Socket),
            Result;
        {error, Reason} ->
            {error, Reason}
    end.

udp_send_to(Config, Addr, Request) ->
    dump(Request, Config#config.debug),

    case gen_udp:open(0, [inet, binary, {active, true}]) of
        {ok, Socket} ->
            Result = send(udp, Socket, Addr, Request, Config#config.debug),
            gen_udp:close(Socket),
            Result;
        {error, Reason} ->
            {error, Reason}
    end.

tcp_send_to(Config,{Addr,Port}, Request) ->
    dump(Request, Config#config.debug),

    case gen_tcp:connect(Addr, Port, [inet, binary, {active, true}], ?CONNECT_TIMEOUT) of
        {ok, Socket} ->
            Result = send(tcp,Socket, Addr, Request, Config#config.debug),
            gen_tcp:close(Socket),
            Result;
        {error, Reason} ->
            {error, Reason}
    end.


% set-ip doesn't return a reply
send(Transport, Socket, DestAddr, <<16#17, 16#96, R/binary>>, _Debug) ->
    case sendto(Transport, Socket, DestAddr, <<16#17, 16#96, R/binary>>) of
        ok ->
            erlang:send_after(?READ_TIMEOUT, self(), timeout),
            {ok, none};
        {error, Reason} ->
            {error, Reason}
    end;

send(Transport, Socket, DestAddr, Request, Debug) ->
    case sendto(Transport, Socket, DestAddr, Request) of
        ok ->
            erlang:send_after(?READ_TIMEOUT, self(), timeout),
            read(Transport, Debug);
        {error, Reason} ->
            {error, Reason}
    end.


listen(Config, Handler) ->
    {Addr, Port} = Config#config.listen,

    case gen_udp:open(Port, [inet, binary, {active, false}, {ip, Addr}]) of
        {ok, Socket} ->
            spawn(fun() ->
                listen(Socket, Handler, Config#config.debug),
                gen_udp:close(Socket),
                Handler ! closed
            end),
            {ok, fun() -> gen_udp:close(Socket) end};
        {error, Reason} ->
            {error, Reason}
    end.


listen(Socket, Handler, Debug) ->
    case gen_udp:recv(Socket, 64) of
        {ok, {_, _, Packet}} ->
            dump(Packet, Debug),
            Handler ! {ok, Packet},
            listen(Socket, Handler, Debug);
        {error, closed} ->
            log:infof(?LOG_TAG, closed),
            {error, closed};
        {error, Reason} ->
            log:infof(?LOG_TAG, io_lib:format("error ~p", [Reason])),
            {error, Reason}
    end.

sendto(tcp, Socket, _, Request) ->
    case inet:setopts(Socket, [{send_timeout, 1000}]) of
        ok ->
            gen_tcp:send(Socket, Request);
        {error, Reason} ->
            {error, Reason}
    end;

sendto(_, Socket, DestAddr, Request) ->
    case inet:setopts(Socket, [{send_timeout, 1000}]) of
        ok ->
            gen_udp:send(Socket, DestAddr, Request);
        {error, Reason} ->
            {error, Reason}
    end.


read_all(Debug) ->
    read_all([], Debug).

read_all(Received, Debug) ->
    receive
        {udp, _, _, _, Packet} ->
            dump(Packet, Debug),
            read_all([Packet | Received], Debug);
        timeout ->
            {ok, Received}
    end.


read(tcp, Debug) ->
    receive
        {tcp, _, Packet} ->
            dump(Packet, Debug),
            {ok, Packet};
        timeout ->
            {error, timeout}
    end;

read(_, Debug) ->
    receive
        {udp, _, _, _, Packet} ->
            dump(Packet, Debug),
            {ok, Packet};
        timeout ->
            {error, timeout}
    end.

resolve(S) ->
    case re:run(S, "([0-9]+[.][0-9]+[.][0-9]+[.][0-9]+)(?::([0-9]+))?", [{capture, all_but_first, list}]) of
        {match, [Address]} ->
            {ok, Addr} = inet:parse_ipv4strict_address(Address),
            {ok, {Addr, 60000}};
        {match, [Address, Port]} ->
            {ok, Addr} = inet:parse_address(Address),
            {ok, {Addr, list_to_integer(Port)}};
        _ ->
            inet:parse_ipv4strict_address(S)
    end.


dump(Packet, true) ->
    bin2hex(Packet, 0);
dump(_, _) ->
    ok.


bin2hex(<<P:8/binary, Q:8/binary, Rest/binary>>, Offset) ->
    <<P1:8, P2:8, P3:8, P4:8, P5:8, P6:8, P7:8, P8:8>> = P,
    <<Q1:8, Q2:8, Q3:8, Q4:8, Q5:8, Q6:8, Q7:8, Q8:8>> = Q,

    F = "~2.16.0B ~2.16.0B ~2.16.0B ~2.16.0B ~2.16.0B ~2.16.0B ~2.16.0B ~2.16.0B",

    io:fwrite("   ~8.16.0B  ~s  ~s~n", [
        Offset,
        io_lib:format(F, [P1, P2, P3, P4, P5, P6, P7, P8]),
        io_lib:format(F, [Q1, Q2, Q3, Q4, Q5, Q6, Q7, Q8])
    ]),

    bin2hex(Rest, Offset + 16);

bin2hex(_, _) ->
    io:fwrite("~n"),
    ok.