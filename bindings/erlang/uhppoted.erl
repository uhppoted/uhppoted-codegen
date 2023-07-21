-module(uhppoted).

-export([ 
    get_all_controllers/1, 
    get_controller/2,
    listen/2
    ]).

-define (LOG_TAG, "uhppoted").

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


listen(Config, Handler) ->
    PID = spawn(fun() -> listen(Handler) end),

    case udp:listen(Config, PID) of
      {ok, F} ->
        {ok, F};

      {error, Reason} ->
        PID ! cancel,
        {error, Reason}
    end.

listen(Handler) ->
    receive 
      {ok, Packet} ->
          case decoder:event(Packet) of 
            {ok, Event} ->
              Handler ! {event, Event};

            Oops ->
              Handler ! {error, Oops}
          end,
          listen(Handler);

      cancel ->
          log:infof(?LOG_TAG,"cancelled"),
          Handler ! closed;

      close ->
          log:infof(?LOG_TAG,"closed"),
          Handler ! closed;

      Any ->
          log:debugf(?LOG_TAG,Any),
          Handler ! closed
    end.    

