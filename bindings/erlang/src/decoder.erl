-module(decoder).

-export([ 
    {{- range .model.responses}}{{template "export" .}},{{end}}
    {{- template "export" .model.event }}
]).

-include("records.hrl").

{{range .model.responses}}
{{ template "decode" . -}}
{{end}}

{{with .model.event}}{{template "decode" .}}
{{end}}

{{define "export"}}
    {{snakeCase .name}}/1{{end -}}

{{define "decode"}}
{{snakeCase .name}}(Packet) when byte_size(Packet) /= 64 ->
    {error, {bad_reply_packet_length, byte_size(Packet)}};

{{snakeCase .name}}(<<_:8, F:8, _/binary>>) when F /= {{.msgtype}} ->
    {error, {bad_reply_function_code, io_lib:format("~2.16.0B", [F])}};

% Ref. v6.62 firmware event
{{snakeCase .name}}(<<SOM:8, F:8, _/binary>>) when (SOM /= 16#17 andalso ((SOM /= 16#19) or (F /= 16#20))) ->
    {error, {bad_reply_start_of_message, io_lib:format("~2.16.0B", [SOM])}};

{{snakeCase .name}}(Packet) ->
    {ok, #{{snakeCase .name}}{
        {{- range $ix,$field := (subslice .fields)}}
        {{snakeCase $field.name}} = unpack({{snakeCase $field.type}}, Packet, {{$field.offset}}),
        {{- end}}
        {{- range $ix,$field := (last .fields)}}
        {{snakeCase $field.name}} = unpack({{snakeCase $field.type}}, Packet, {{$field.offset}})
        {{- end}}
    }}.
{{end}}

unpack(bool, Packet, Offset) ->
    <<_:Offset/binary, B:1/binary, _/binary>> = Packet,
    case binary:decode_unsigned(B, little) of
        1 ->
            true;
        _ ->
            false
    end;

unpack(uint8, Packet, Offset) ->
    <<_:Offset/binary, B:1/binary, _/binary>> = Packet,
    binary:decode_unsigned(B, little);

unpack(uint16, Packet, Offset) ->
    <<_:Offset/binary, B:2/binary, _/binary>> = Packet,
    binary:decode_unsigned(B, little);

unpack(uint32, Packet, Offset) ->
    <<_:Offset/binary, B:4/binary, _/binary>> = Packet,
    binary:decode_unsigned(B, little);

unpack(ipv4, Packet, Offset) ->
    <<_:Offset/binary, B:4/binary, _/binary>> = Packet,
    <<B1, B2, B3, B4>> = B,
    {B1, B2, B3, B4};

unpack(mac, Packet, Offset) ->
    <<_:Offset/binary, B:6/binary, _/binary>> = Packet,
    <<B1, B2, B3, B4, B5, B6>> = B,
    io_lib:format("~2.16.0B:~2.16.0B:~2.16.0B:~2.16.0B:~2.16.0B:~2.16.0B", [B1, B2, B3, B4, B5, B6]);

unpack(version, Packet, Offset) ->
    <<_:Offset/binary, B:2/binary, _/binary>> = Packet,
    <<Major, Minor>> = B,
    io_lib:format("~2.16.0B~2.16.0B", [Major, Minor]);

unpack(date, Packet, Offset) ->
    <<_:Offset/binary, B:4/binary, _/binary>> = Packet,
    <<YYYY:2/binary, MM:1/binary, DD:1/binary>> = B,
    Year = bcd_to_integer(YYYY),
    Month = bcd_to_integer(MM),
    Day = bcd_to_integer(DD),
    {Year, Month, Day};

unpack(optional_date, Packet, Offset) ->
    <<_:Offset/binary, B:4/binary, _/binary>> = Packet,
    <<YYYY:2/binary, MM:1/binary, DD:1/binary>> = B,
    Year = bcd_to_integer(YYYY),
    Month = bcd_to_integer(MM),
    Day = bcd_to_integer(DD),
    {Year, Month, Day};

unpack(shortdate, Packet, Offset) ->
    <<_:Offset/binary, B:3/binary, _/binary>> = Packet,
    <<YY:1/binary, MM:1/binary, DD:1/binary>> = B,

    Year = bcd_to_integer(YY),
    Month = bcd_to_integer(MM),
    Day = bcd_to_integer(DD),

    {2000 + Year, Month, Day};

unpack(time, Packet, Offset) ->
    <<_:Offset/binary, B:3/binary, _/binary>> = Packet,
    <<HH:1/binary, Mm:1/binary, SS:1/binary>> = B,

    Hour = bcd_to_integer(HH),
    Minute = bcd_to_integer(Mm),
    Second = bcd_to_integer(SS),

    {Hour, Minute, Second};

unpack(datetime, Packet, Offset) ->
    <<_:Offset/binary, B:7/binary, _/binary>> = Packet,
    <<YYYY:2/binary, MM:1/binary, DD:1/binary, HH:1/binary, Mm:1/binary, SS:1/binary>> = B,

    Year = bcd_to_integer(YYYY),
    Month = bcd_to_integer(MM),
    Day = bcd_to_integer(DD),
    Hour = bcd_to_integer(HH),
    Minute = bcd_to_integer(Mm),
    Second = bcd_to_integer(SS),

    { {Year, Month, Day}, {Hour, Minute, Second}};

unpack(optional_datetime, Packet, Offset) ->
    <<_:Offset/binary, B:7/binary, _/binary>> = Packet,
    <<YYYY:2/binary, MM:1/binary, DD:1/binary, HH:1/binary, Mm:1/binary, SS:1/binary>> = B,

    Year = bcd_to_integer(YYYY),
    Month = bcd_to_integer(MM),
    Day = bcd_to_integer(DD),
    Hour = bcd_to_integer(HH),
    Minute = bcd_to_integer(Mm),
    Second = bcd_to_integer(SS),

    { {Year, Month, Day}, {Hour, Minute, Second}};

unpack(hhmm, Packet, Offset) ->
    <<_:Offset/binary, B:2/binary, _/binary>> = Packet,
    <<HH:1/binary, MM:1/binary>> = B,

    Hour = bcd_to_integer(HH),
    Minute = bcd_to_integer(MM),

    {Hour, Minute, 0};

unpack(pin, Packet, Offset) ->
    <<_:Offset/binary, B:3/binary, _/binary>> = Packet,
    binary:decode_unsigned(B, little).

bcd_to_integer(BCD) ->
    Bytes = [<<B>> || B <- binary_to_list(BCD)],
    Nibbles = lists:flatten([[N1, N2] || <<N1:4/bits, N2:4/bits>> <- Bytes]),
    lists:foldl(fun(<<N:4>>, I) -> 10 * I + N end, 0, Nibbles).
