-module(commands).

-export([commands/0, find/1, exec/3]).

-define(CONTROLLER, 405419896).
-define(DOOR, 3).
-define(MODE, 2).
-define(DELAY, 10).
-define(CARD, 10058400).
-define(CARD_INDEX, 1).
-define(EVENT_INDEX, 200).
-define(TIME_PROFILE_ID, 29).

-define(ADDRESS, "192.168.1.100").
-define(NETMASK, "255.255.255.0").
-define(GATEWAY, "192.168.1.1").
-define(LISTENER, "192.168.1.100:60001").

-define(LOG_TAG, "commands").

-include("records.hrl").

commands() ->
    [
        {"get-all-controllers", get_all_controllers},
        {"get-controller", get_controller},
        {"set-ip", set_ip},
        {"get-time", get_time},
        {"set-time", set_time},
        {"get-status", get_status},
        {"get-listener", get_listener},
        {"set-listener", set_listener},
        {"get-door-control", get_door_control},
        {"set-door-control", set_door_control},
        {"open-door", open_door},
        {"get-cards", get_cards},
        {"get-card", get_card},
        {"get-card-by-index", get_card_by_index},
        {"put-card", put_card},
        {"delete-card", delete_card},
        {"delete-all-cards", delete_all_cards},
        {"get-event", get_event},
        {"get-event-index", get_event_index},
        {"set-event-index", set_event_index},
        {"record-special-events", record_special_events},
        {"get-time-profile", get_time_profile},
        {"set-time-profile", set_time_profile},
        {"delete-all-time-profiles", delete_all_time_profiles},
        {"add-task", add_task},
        {"refresh-tasklist", refresh_tasklist},
        {"clear-tasklist", clear_tasklist},
        {"set-pc-control", set_pc_control},
        {"set-interlock", set_interlock},
        {"activate-keypads", activate_keypads},
        {"set-door-passcodes", set_door_passcodes},
        {"restore-default-parameters", restore_default_parameters},
        {"listen", listen}
    ].

find(Cmd) ->
    lists:keyfind(Cmd, 1, commands()).

exec({_, Cmd}, Options, Config) ->
    case execute(Cmd, Options, Config) of
        {ok, none} ->
            io:format("   ok~n");

        {ok, Any} ->
            pprint({ok, Any});

        Controllers when is_list(Controllers) ->
            lists:foreach(fun(X) -> pprint(X) end, Controllers);

        Other ->
            io:format(">> ~p~n", [Other])
    end.

execute(get_all_controllers, _Options, Config) ->
    uhppoted:get_all_controllers(Config);

execute(get_controller, _Options, Config) ->
    Controller = ?CONTROLLER,
    uhppoted:get_controller(Config, Controller);

execute(set_ip, _Options, Config) ->
    Controller = ?CONTROLLER,
    {ok, Address} = inet:parse_ipv4_address(?ADDRESS),
    {ok, Netmask} = inet:parse_ipv4_address(?NETMASK),
    {ok, Gateway} = inet:parse_ipv4_address(?GATEWAY),

    uhppoted:set_ip(Config, Controller, Address, Netmask, Gateway);

execute(get_time, _Options, Config) ->
    Controller = ?CONTROLLER,
    uhppoted:get_time(Config, Controller);

execute(set_time, _Options, Config) ->
    Controller = ?CONTROLLER,
    Now = erlang:localtime(),

    uhppoted:set_time(Config, Controller, Now);

execute(get_status, _Options, Config) ->
    Controller = ?CONTROLLER,
    uhppoted:get_status(Config, Controller);

execute(get_listener, _Options, Config) ->
    Controller = ?CONTROLLER,
    uhppoted:get_listener(Config, Controller);

execute(set_listener, _Options, Config) ->
    Controller = ?CONTROLLER,
    {Addr, Port} = parse_addr(?LISTENER),
    uhppoted:set_listener(Config, Controller, Addr, Port);

execute(get_door_control, _Options, Config) ->
    Controller = ?CONTROLLER,
    Door = ?DOOR,
    uhppoted:get_door_control(Config, Controller, Door);

execute(set_door_control, _Options, Config) ->
    Controller = ?CONTROLLER,
    Door = ?DOOR,
    Mode = ?MODE,
    Delay = ?DELAY,
    uhppoted:set_door_control(Config, Controller, Door, Mode, Delay);

execute(open_door, _Options, Config) ->
    Controller = ?CONTROLLER,
    Door = ?DOOR,
    uhppoted:open_door(Config, Controller, Door);

execute(get_cards, _Options, Config) ->
    Controller = ?CONTROLLER,
    uhppoted:get_cards(Config, Controller);

execute(get_card, _Options, Config) ->
    Controller = ?CONTROLLER,
    Card = ?CARD,
    uhppoted:get_card(Config, Controller, Card);

execute(get_card_by_index, _Options, Config) ->
    Controller = ?CONTROLLER,
    Index = ?CARD_INDEX,
    uhppoted:get_card_by_index(Config, Controller, Index);

execute(put_card, _Options, Config) ->
    Controller = ?CONTROLLER,
    Card = ?CARD,
    Start = {2023, 1, 1},
    End = {2023, 12, 31},
    PIN = 7531,
    uhppoted:put_card(Config, Controller, Card, Start, End, 0, 1, 29, 0, PIN);

execute(delete_card, _Options, Config) ->
    Controller = ?CONTROLLER,
    Card = ?CARD,
    uhppoted:delete_card(Config, Controller, Card);

execute(delete_all_cards, _Options, Config) ->
    Controller = ?CONTROLLER,
    uhppoted:delete_all_cards(Config, Controller);

execute(get_event, _Options, Config) ->
    Controller = ?CONTROLLER,
    Index = ?EVENT_INDEX,
    uhppoted:get_event(Config, Controller, Index);

execute(get_event_index, _Options, Config) ->
    Controller = ?CONTROLLER,
    uhppoted:get_event_index(Config, Controller);

execute(set_event_index, _Options, Config) ->
    Controller = ?CONTROLLER,
    Index = ?EVENT_INDEX,
    uhppoted:set_event_index(Config, Controller, Index);

execute(record_special_events, _Options, Config) ->
    Controller = ?CONTROLLER,
    Enable = true,
    uhppoted:record_special_events(Config, Controller, Enable);

execute(get_time_profile, _Options, Config) ->
    Controller = ?CONTROLLER,
    Profile = ?TIME_PROFILE_ID,
    uhppoted:get_time_profile(Config, Controller, Profile);

execute(set_time_profile, _Options, Config) ->
    Controller = ?CONTROLLER,
    Profile = ?TIME_PROFILE_ID,
    Start = {2023, 1, 1},
    End = {2023, 12, 31},
    Monday = true,
    Tuesday = true,
    Wednesday = false,
    Thursday = true,
    Friday = false,
    Saturday = false,
    Sunday = true,
    Segment1Start = {8, 30},
    Segment1End = {11, 45},
    Segment2Start = {13, 15},
    Segment2End = {16, 30},
    Segment3Start = {19, 30},
    Segment3End = {20, 55},
    LinkedProfileID = 30,

    uhppoted:set_time_profile(
        Config,
        Controller,
        Profile,
        Start,
        End,
        Monday,
        Tuesday,
        Wednesday,
        Thursday,
        Friday,
        Saturday,
        Sunday,
        Segment1Start,
        Segment1End,
        Segment2Start,
        Segment2End,
        Segment3Start,
        Segment3End,
        LinkedProfileID
    );

execute(delete_all_time_profiles, _Options, Config) ->
    Controller = ?CONTROLLER,
    uhppoted:delete_all_time_profiles(Config, Controller);

execute(add_task, _Options, Config) ->
    Controller = ?CONTROLLER,
    Door = ?DOOR,
    TaskType = 2,
    Start = {2023, 1, 1},
    End = {2023, 12, 31},
    Monday = true,
    Tuesday = false,
    Wednesday = true,
    Thursday = true,
    Friday = false,
    Saturday = false,
    Sunday = true,
    StartTime = {8, 30},
    MoreCards = 0,

    uhppoted:add_task(
        Config,
        Controller,
        Start,
        End,
        Monday,
        Tuesday,
        Wednesday,
        Thursday,
        Friday,
        Saturday,
        Sunday,
        StartTime,
        Door,
        TaskType,
        MoreCards
    );

execute(refresh_tasklist, _Options, Config) ->
    Controller = ?CONTROLLER,
    uhppoted:refresh_tasklist(Config, Controller);

execute(clear_tasklist, _Options, Config) ->
    Controller = ?CONTROLLER,
    uhppoted:clear_tasklist(Config, Controller);

execute(set_pc_control, _Options, Config) ->
    Controller = ?CONTROLLER,
    Enable = true,
    uhppoted:set_pc_control(Config, Controller, Enable);

execute(set_interlock, _Options, Config) ->
    Controller = ?CONTROLLER,
    Interlock = 3,
    uhppoted:set_interlock(Config, Controller, Interlock);

execute(activate_keypads, _Options, Config) ->
    Controller = ?CONTROLLER,
    Reader1 = true,
    Reader2 = true,
    Reader3 = false,
    Reader4 = true,
    uhppoted:activate_keypads(Config, Controller, Reader1, Reader2, Reader3, Reader4);

execute(set_door_passcodes, _Options, Config) ->
    Controller = ?CONTROLLER,
    Door = ?DOOR,
    Passcode1 = 12345,
    Passcode2 = 0,
    Passcode3 = 999999,
    Passcode4 = 54321,
    uhppoted:set_door_passcodes(Config, Controller, Door, Passcode1, Passcode2, Passcode3, Passcode4);

execute(restore_default_parameters, _Options, Config) ->
    Controller = ?CONTROLLER,
    uhppoted:restore_default_parameters(Config, Controller);

execute(listen, _Options, Config) ->
    case uhppoted:listen(Config, self()) of
        {ok, F} ->
            % in lieu of a CTRL-C handler (or more properly an OTP supervision tree)
            io:format("(type Q to quit)~n~n"),
            spawn(fun() ->
                io:fread("", "c"),
                F()
            end),
            listen();
        {error, Reason} ->
            {error, Reason}
    end;

execute(C, _, _) ->
    erlang:error({not_implemented, C}).

listen() ->
    receive
        {event, Event} ->
            pprint({event, Event}),
            listen();
        {error, Reason} ->
            log:errorf(?LOG_TAG, Reason),
            listen();
        closed ->
            log:infof(?LOG_TAG, closed)
    end.

parse_addr(S) ->
    [A, P] = string:tokens(S, ":"),
    {ok, Addr} = inet:parse_address(A),
    {Port, _} = string:to_integer(P),
    {Addr, Port}.

% Ref. https://erlang.org/pipermail/erlang-questions/2008-November/040029.html
pprint({ok, Response}) ->
    pretty_print(Response);

pprint({event, Event}) ->
    pretty_print(Event).

pretty_print(Record) ->
    [Name | Values] = tuple_to_list(Record),
    Fields = fields(Name),
    W = io_lib:format("       ~~-~Bs  ~~p~~n", [width(Fields)]),
    io:format("   ~s~n", [Name]),
    lists:foreach(fun({F, V}) -> io:format(W, [F, V]) end, zip(Fields, Values)),
    io:format("~n").

width(Fields) ->
    B = [atom_to_binary(X) || X <- Fields],
    N = [byte_size(X) || X <- B],
    lists:foldl(fun(X, W) -> max(X, W) end, 0, N).

zip(Fields, Values) ->
    zip(Fields, Values, []).

zip([], _, L) ->
    lists:reverse(L);

zip(_, [], L) ->
    lists:reverse(L);

zip([F | Fields], [V | Values], L) ->
    zip(Fields, Values, [{F, V} | L]).

{{range .model.responses}}
fields({{snakeCase .name}}) ->
    record_info(fields, {{snakeCase .name}});
{{end}}

{{with .model.event}}
fields({{snakeCase .name}}) ->
    record_info(fields, {{snakeCase .name}});
{{end}}

fields(_) ->
    [].



