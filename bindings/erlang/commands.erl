-module(commands).

-export([commands/0, find/1, exec/3]).

-define(CONTROLLER, 405419896).
-define(DOOR, 3).
-define(MODE, 2).
-define(DELAY, 10).
-define(CARD, 10058400).
-define(CARD_INDEX, 3).
-define(EVENT_INDEX, 37).
-define(TIME_PROFILE_ID, 29).

-define(ADDRESS,  "192.168.1.100" ).
-define(NETMASK,  "255.255.255.0" ).
-define(GATEWAY,  "192.168.1.1" ).
-define(LISTENER, "192.168.1.100:6001" ).

-define (LOG_TAG, "commands").

commands() ->
    [ 
      { "get-all-controllers", get_all_controllers },
      { "get-controller", get_controller },
      { "set-ip", set_ip },
      { "get-time", get_time },
      { "listen", listen }
    ].

find(Cmd) ->
    lists:keyfind(Cmd,1,commands()).

exec({_, Cmd}, Options, Config) ->
    io:format(">> ~p~n", [ execute(Cmd, Options, Config) ]).

execute(get_all_controllers, _Options, Config) ->
    uhppoted:get_all_controllers(Config);

execute(get_controller, _Options, Config) ->
    Controller = ?CONTROLLER,
    uhppoted:get_controller(Config, Controller);

execute(set_ip, _Options, Config) ->
    Controller = ?CONTROLLER,
    {ok, Address } = inet:parse_ipv4_address(?ADDRESS),
    {ok, Netmask } = inet:parse_ipv4_address(?NETMASK),
    {ok, Gateway } = inet:parse_ipv4_address(?GATEWAY),

    uhppoted:set_ip(Config, Controller, Address, Netmask, Gateway);

execute(get_time, _Options, Config) ->
    Controller = ?CONTROLLER,
    uhppoted:get_time(Config, Controller);

execute(listen, _Options, Config) ->
    case uhppoted:listen(Config, self()) of 
      {ok, F} ->
        spawn(fun() -> io:fread("(type Q to quit)  ","c"), F() end), % in lieu of a CTRL-C handler (or more properly an OTP supervision tree)
        listen();

      {error, Reason} ->
        {error, Reason}
    end;

execute(C, _, _) ->
    erlang:error({not_implemented, C}).

listen() ->
    receive
      { event,Event } ->
        pprint({ event, Event }),
        listen();

      { error,Reason } ->
        log:errorf(?LOG_TAG, Reason),
        listen();

      closed ->
        log:infof(?LOG_TAG, closed)
    end.

% pprint({ok, Any}) ->
%     io:format("RESPONSE  ~p~n", [ Any ]);

pprint({event, Event}) ->
    io:format("EVENT: ~p~n", [ Event ]).

