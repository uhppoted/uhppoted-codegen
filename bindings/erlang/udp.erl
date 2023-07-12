-module(udp).

-export([ broadcast/2 ]).

-record(config, { bind, broadcast, listen, debug }).

broadcast (Config, Request) ->
    dump(Request, Config#config.debug),

    broadcast(gen_udp:open(0, [binary, {active,false}]),
         Config#config.broadcast,  
         Request).

broadcast ({ok, Socket}, DestAddr, Request) ->
    P = inet:setopts(Socket, [{broadcast, true}]),
    io:format(">>>>>>> ~p~n",[P]),

    Q = inet:setopts(Socket, [{send_timeout, 1000}]),
    io:format(">>>>>>> ~p~n",[Q]),

    X = gen_udp:send(Socket, DestAddr, Request),
    io:format(">>>>>>> ~p~n",[X]),

    {ok,[]};

broadcast ({error, Reason},_,_s) ->
    {error, Reason}.


% func broadcast(request []byte) ([][]byte, error) {
%     dump(request)
% 
%     socket, err := net.ListenUDP("udp", bindAddr)
%     if err != nil {
%         return nil, err
%     }
% 
%     defer socket.Close()
% 
%     if err := socket.SetWriteDeadline(time.Now().Add(WRITE_TIMEOUT)); err != nil {
%         return nil, err
%     }
% 
%     if err := socket.SetReadDeadline(time.Now().Add(READ_TIMEOUT)); err != nil {
%         return nil, fmt.Errorf("Failed to set UDP read timeout [%v]", err)
%     }
% 
%     if _, err := socket.WriteToUDP(request, destAddr); err != nil {
%         return nil, err
%     }
% 
%     return readAll(socket)
% }

dump(Request, true) ->
    bin2hex(Request,0);

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
    ok.

