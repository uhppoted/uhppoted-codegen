-module(decoder_test).

-include_lib("eunit/include/eunit.hrl").
-include("records.hrl").

get_status_test() ->
    Packet =
        <<16#17, 16#20, 16#00, 16#00, 16#78, 16#37, 16#2a, 16#18, 16#4e, 16#00, 16#00, 16#00, 16#02, 16#01, 16#03, 16#01,
          16#a1, 16#98, 16#7c, 16#00, 16#20, 16#22, 16#08, 16#23, 16#09, 16#47, 16#06, 16#2c, 16#00, 16#01, 16#00, 16#00,
          16#00, 16#00, 16#00, 16#01, 16#03, 16#09, 16#49, 16#39, 16#00, 16#00, 16#00, 16#00, 16#00, 16#00, 16#00, 16#00,
          16#27, 16#07, 16#09, 16#22, 16#08, 16#23, 16#00, 16#00, 16#00, 16#00, 16#00, 16#00, 16#00, 16#00, 16#00, 16#00>>,

    {ok, Response} = decoder:get_status_response(Packet),

    ?assertEqual(405419896, Response#get_status_response.controller),
    ?assertEqual({2022, 8, 23}, Response#get_status_response.system_date),
    ?assertEqual({9, 49, 39}, Response#get_status_response.system_time),
    ?assertEqual(false, Response#get_status_response.door_1_open),
    ?assertEqual(true, Response#get_status_response.door_2_open),
    ?assertEqual(false, Response#get_status_response.door_3_open),
    ?assertEqual(false, Response#get_status_response.door_4_open),
    ?assertEqual(false, Response#get_status_response.door_1_button),
    ?assertEqual(false, Response#get_status_response.door_2_button),
    ?assertEqual(false, Response#get_status_response.door_3_button),
    ?assertEqual(true, Response#get_status_response.door_4_button),
    ?assertEqual(7, Response#get_status_response.relays),
    ?assertEqual(9, Response#get_status_response.inputs),
    ?assertEqual(3, Response#get_status_response.system_error),
    ?assertEqual(39, Response#get_status_response.special_info),
    ?assertEqual(78, Response#get_status_response.event_index),
    ?assertEqual(2, Response#get_status_response.event_type),
    ?assertEqual(true, Response#get_status_response.event_access_granted),
    ?assertEqual(3, Response#get_status_response.event_door),
    ?assertEqual(1, Response#get_status_response.event_direction),
    ?assertEqual(8165537, Response#get_status_response.event_card),
    ?assertEqual({ {2022, 8, 23}, {9, 47, 6} }, Response#get_status_response.event_timestamp),
    ?assertEqual(44, Response#get_status_response.event_reason),
    ?assertEqual(0, Response#get_status_response.sequence_no).

get_status_without_event_test() ->
    Packet =
        <<16#17, 16#20, 16#00, 16#00, 16#78, 16#37, 16#2a, 16#18, 16#00, 16#00, 16#00, 16#00, 16#00, 16#00, 16#00, 16#00,
          16#00, 16#00, 16#00, 16#00, 16#00, 16#00, 16#00, 16#00, 16#00, 16#00, 16#00, 16#00, 16#00, 16#01, 16#00, 16#00,
          16#00, 16#00, 16#00, 16#01, 16#03, 16#09, 16#49, 16#39, 16#00, 16#00, 16#00, 16#00, 16#00, 16#00, 16#00, 16#00,
          16#27, 16#07, 16#09, 16#22, 16#08, 16#23, 16#00, 16#00, 16#00, 16#00, 16#00, 16#00, 16#00, 16#00, 16#00, 16#00>>,

    {ok, Response} = decoder:get_status_response(Packet),

    ?assertEqual(405419896, Response#get_status_response.controller),
    ?assertEqual({2022, 8, 23}, Response#get_status_response.system_date),
    ?assertEqual({9, 49, 39}, Response#get_status_response.system_time),
    ?assertEqual(false, Response#get_status_response.door_1_open),
    ?assertEqual(true, Response#get_status_response.door_2_open),
    ?assertEqual(false, Response#get_status_response.door_3_open),
    ?assertEqual(false, Response#get_status_response.door_4_open),
    ?assertEqual(false, Response#get_status_response.door_1_button),
    ?assertEqual(false, Response#get_status_response.door_2_button),
    ?assertEqual(false, Response#get_status_response.door_3_button),
    ?assertEqual(true, Response#get_status_response.door_4_button),
    ?assertEqual(7, Response#get_status_response.relays),
    ?assertEqual(9, Response#get_status_response.inputs),
    ?assertEqual(3, Response#get_status_response.system_error),
    ?assertEqual(39, Response#get_status_response.special_info),
    ?assertEqual(0, Response#get_status_response.event_index),
    ?assertEqual(0, Response#get_status_response.event_type),
    ?assertEqual(false, Response#get_status_response.event_access_granted),
    ?assertEqual(0, Response#get_status_response.event_door),
    ?assertEqual(0, Response#get_status_response.event_direction),
    ?assertEqual(0, Response#get_status_response.event_card),
    ?assertEqual({ {0, 0, 0}, {0, 0, 0} }, Response#get_status_response.event_timestamp),
    ?assertEqual(0, Response#get_status_response.event_reason),
    ?assertEqual(0, Response#get_status_response.sequence_no).
