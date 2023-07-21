-module(commands).

-export([commands/0, find/1, exec/2]).

-define(CONTROLLER, 405419896).
-define (LOG_TAG, "commands").

commands() ->
    [ 
      { "get-all-controllers", get_all_controllers },
      { "get-controller", get_controller },
      { "listen", listen }
    ].

find(Cmd) ->
    lists:keyfind(Cmd,1,commands()).

exec({_, Cmd}, Config) ->
    io:format(">> ~p~n", [ execute(Cmd, Config) ]).

execute(get_all_controllers, Config) ->
    uhppoted:get_all_controllers(Config);

execute(get_controller, Config) ->
    Controller = ?CONTROLLER,
    uhppoted:get_controller(Config, Controller);

execute(listen, Config) ->
    case uhppoted:listen(Config, self()) of 
      {ok, PID} ->
        spawn(fun() ->
          io:fread("(type Q to quit)  ","c"),
          PID ! close
        end),

        spawn(fun() -> timer:sleep(30000), PID ! close end),
        listen(PID);

      {error, Reason} ->
        {error, Reason}
    end;

execute(C, _) ->
    erlang:error({not_implemented, C}).

listen(PID) ->
    receive
      { event,Event } ->
        pprint({ event, Event }),
        listen(PID);

      { error,Reason } ->
        log:errorf(?LOG_TAG, Reason),
        listen(PID);

      closed ->
        log:infof(?LOG_TAG, "closed")
    end.

% pprint({ok, Any}) ->
%     io:format("RESPONSE  ~p~n", [ Any ]);

pprint({event, Event}) ->
    io:format("EVENT: ~p~n", [ Event ]).

