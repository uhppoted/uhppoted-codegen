-module(uhppoted).

-export([ 
    get_all_controllers/1, 
    get_controller/2,
    listen/1
    ]).

get_all_controllers (Config) ->
    Request = encoder:get_controller_request(0),
    
    case udp:broadcast(Config, Request) of
      {ok, Received} ->
        [ decoder:get_controller_response(P) || P <- Received ];

      {error, Reason} ->
        {error, Reason}
    end.

get_controller (Config, Controller) ->
    Request = encoder:get_controller_request(Controller),
    
    case udp:send(Config, Request) of
      {ok, Received} ->
        decoder:get_controller_response(Received);

      {error, Reason} ->
        {error, Reason}
    end.

listen(Config) ->
    case udp:listen(Config, self()) of
      {ok, woot} ->
        listen();

      {error, Reason} ->
        {error, Reason}
    end.

listen() ->
    receive 
      {ok, Packet} ->
          case decoder:event(Packet) of 
            {ok, Event} ->
              io:format(">>>>>>> EVENT ~p~n", [Event]);

            Oops ->
              io:format(">>>>>>> OOOPS ~p~n", [Oops])
          end,
          listen();

      Any ->
        Any
    end.    