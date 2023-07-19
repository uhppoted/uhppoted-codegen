-module(decoder).

-export([ 
    get_controller_response/1,
    event/1
]).

-record(uhppote_controller, { 
    controller,
    address,
    netmask,
    gateway,
    mac,
    version,
    date
 }).

-record(uhppote_event, { 
    controller,
    event_index,
    event_type,
    event_access_granted,
    event_door,
    event_direction,
    event_card,
    event_timestamp,
    event_reason,
    system_date,
    system_time,
    system_error,
    door_1_open,
    door_2_open,
    door_3_open,
    door_4_open,
    door_1_button,
    door_2_button,
    door_3_button,
    door_4_button,
    relays,
    inputs,
    special_info,
    sequence_number
 }).

get_controller_response (<< 16#17, 16#94, 16#00, 16#00, 
                            Controller:4/little-unsigned-integer-unit:8,
                            Address:4/binary,
                            Netmask:4/binary,
                            Gateway:4/binary,
                            MAC:6/binary,
                            Version:2/binary,
                            Date:4/binary,
                            _/binary>>) ->
    { ok, #uhppote_controller{
            controller = Controller,
            address = unpack_IPv4(Address),
            netmask = unpack_IPv4(Netmask),
            gateway = unpack_IPv4(Gateway),
            mac = unpack_MAC(MAC),
            version = unpack_version(Version),
            date = unpack_date(Date)
          }
    };

get_controller_response (_) ->
    { error, invalid_packet }.

event (<< 16#17, 16#20, 16#00, 16#00, 
          Controller:4/binary,
          EventIndex:4/binary,
          EventType:1/binary,
          EventAccessGranted:1/binary,
          EventDoor:1/binary,
          EventDirection:1/binary,
          EventCard:4/binary,
          EventTimestamp:7/binary,
          EventReason:1/binary,
          Door1Open:1/binary,
          Door2Open:1/binary,
          Door3Open:1/binary,
          Door4Open:1/binary,
          Door1Button:1/binary,
          Door2Button:1/binary,
          Door3Button:1/binary,
          Door4Button:1/binary,
          SystemError:1/binary,
          SystemTime:3/binary,
          SequenceNo:4/binary,
          _:4/binary,
          SpecialInfo:1/binary,
          Relays:1/binary,
          Inputs:1/binary,
          SystemDate:3/binary, % offset 51
          _/binary>>) ->
    { ok, #uhppote_event{
            controller = unpack_uint32(Controller),
            event_index = unpack_uint32(EventIndex),
            event_type = unpack_uint8(EventType),
            event_access_granted = unpack_uint8(EventAccessGranted),
            event_door = unpack_uint8(EventDoor),
            event_direction = unpack_uint8(EventDirection),
            event_card = unpack_uint32(EventCard),
            event_timestamp = unpack_datetime(EventTimestamp),
            event_reason = unpack_uint8(EventReason),
            system_date = unpack_shortdate(SystemDate),
            system_time = unpack_time(SystemTime),
            system_error = unpack_uint8(SystemError),
            door_1_open = unpack_bool(Door1Open),
            door_2_open = unpack_bool(Door2Open),
            door_3_open = unpack_bool(Door3Open),
            door_4_open = unpack_bool(Door4Open),
            door_1_button = unpack_bool(Door1Button),
            door_2_button = unpack_bool(Door2Button),
            door_3_button = unpack_bool(Door3Button),
            door_4_button = unpack_bool(Door4Button),
            relays = unpack_uint8(Relays),
            inputs = unpack_uint8(Inputs),
            special_info = unpack_uint8(SpecialInfo),
            sequence_number = unpack_uint32(SequenceNo)
          }
    };

event (_) ->
    { error, invalid_packet }.

% func event(packet []byte) (response *Event, err error) {
%     // Ref. v6.62 firmware event
%     if packet[0] != 0x17 && (packet[0] != 0x19 || packet[1] != 0x20) {
%         err = fmt.Errorf("invalid reply start of message byte (%02x)", packet[0])
%         return
%     }

unpack_bool(B) ->
    case binary:decode_unsigned(B, little) of
        1 ->
            true;
        _ ->
            false
    end.

unpack_uint8(B) ->
    binary:decode_unsigned(B, little).

unpack_uint32(B) ->
    binary:decode_unsigned(B, little).

unpack_IPv4(<<B1,B2,B3,B4>>) ->
    { B1,B2,B3,B4 }.

unpack_MAC(<<B1,B2,B3,B4,B5,B6>>) ->
    io_lib:format("~2.16.0B:~2.16.0B:~2.16.0B:~2.16.0B:~2.16.0B:~2.16.0B", [ B1,B2,B3,B4,B5,B6 ]).

unpack_version(<<Major,Minor>>) -> 
    io_lib:format("~2.16.0B~2.16.0B", [ Major, Minor ]).

unpack_date(<<YYYY:2/binary,MM:1/binary,DD:1/binary>>) ->
    Year  = bcd_to_integer(YYYY),
    Month = bcd_to_integer(MM),
    Day = bcd_to_integer(DD),
    { Year, Month, Day }.

unpack_shortdate(<<YY:1/binary,MM:1/binary, DD:1/binary>>) ->
    Year  = bcd_to_integer(YY),
    Month = bcd_to_integer(MM),
    Day = bcd_to_integer(DD),
    { 2000 + Year, Month, Day }.

unpack_time(<<HH:1/binary,Mm:1/binary,SS:1/binary>>) ->
    Hour = bcd_to_integer(HH),
    Minute = bcd_to_integer(Mm),
    Second = bcd_to_integer(SS),

    { Hour, Minute, Second }.

unpack_datetime(<<YYYY:2/binary,MM:1/binary,DD:1/binary,HH:1/binary,Mm:1/binary,SS:1/binary>>) ->
    Year = bcd_to_integer(YYYY),
    Month = bcd_to_integer(MM),
    Day = bcd_to_integer(DD),
    Hour = bcd_to_integer(HH),
    Minute = bcd_to_integer(Mm),
    Second = bcd_to_integer(SS),

    { {Year, Month, Day}, {Hour, Minute, Second}}.

bcd_to_integer(BCD) ->
    Bytes = [ <<B>> || B <- binary_to_list(BCD) ],
    Nibbles = lists:flatten([ [N1,N2] || <<N1:4/bits,N2:4/bits>> <- Bytes]),
    lists:foldl(fun(<<N:4>>, I) -> 10*I + N end, 0, Nibbles).
