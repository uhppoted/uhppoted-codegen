-module(decoder).

-export([ 
    {{- template "export" (index .model.responses 0) }},
    {{- template "export" .model.event }}
]).

{{define "export"}}
    {{snakeCase .name}}/1{{end -}}

{{ template "response" (index .model.responses 0) -}}

{{with .model.event}}
-record({{snakeCase .name}}, {
    {{- range (subslice .fields)}}
    {{snakeCase .name}},{{end}}
    {{range (last .fields)}}{{snakeCase .name}}{{end}}
}).
{{end}}

{{define "response"}}
-record({{snakeCase .name}}, {
    {{- range (subslice .fields)}}
    {{snakeCase .name}},{{end}}
    {{range (last .fields)}}{{snakeCase .name}}{{end}}
}).
{{end}}

{{ template "decode" (index .model.responses 0) -}}

{{with .model.event}}{{template "decode" .}}
{{end}}

{{define "decode"}}
{{snakeCase .name}}(Packet) ->
    % if len(packet) != 64 {
    %     err = fmt.Errorf("invalid reply packet length (%v)", len(packet))
    %     return
    % }

    % // Ref. v6.62 firmware event
    % if packet[0] != 0x17 && (packet[0] != 0x19 || packet[1] != 0x20) {
    %     err = fmt.Errorf("invalid reply start of message byte (%02x)", packet[0])
    %     return
    % }

    % if packet[1] != {{byte2hex .msgtype}} {
    %     err = fmt.Errorf("invalid reply function code (%02x)", packet[1])
    %     return
    % }

    { ok, #{{snakeCase .name}}{
             {{- range $ix,$field := (subslice .fields)}}
             {{snakeCase $field.name}} = unpack({{snakeCase $field.type}}, Packet, {{$field.offset}}),
             {{- end}}
             {{- range $ix,$field := (last .fields)}}
             {{snakeCase $field.name}} = unpack({{snakeCase $field.type}}, Packet, {{$field.offset}})
             {{- end}}
          }
    }.
{{end}}

unpack(bool, Packet, Offset) ->
    <<_:Offset/binary,B:1/binary,_/binary>> = Packet,
    case binary:decode_unsigned(B, little) of
        1 ->
            true;
        _ ->
            false
    end;

unpack(uint8, Packet, Offset) ->
    <<_:Offset/binary,B:1/binary,_/binary>> = Packet,
    binary:decode_unsigned(B, little);

unpack(uint32, Packet, Offset) ->
    <<_:Offset/binary,B:4/binary,_/binary>> = Packet,
    binary:decode_unsigned(B, little);

unpack(ipv4, Packet, Offset) ->
    <<_:Offset/binary,B:4/binary,_/binary>> = Packet,
    << B1,B2,B3,B4>> = B,
    { B1,B2,B3,B4 };

unpack(mac, Packet, Offset) ->
    <<_:Offset/binary,B:6/binary,_/binary>> = Packet,
    <<B1,B2,B3,B4,B5,B6>> = B,
    io_lib:format("~2.16.0B:~2.16.0B:~2.16.0B:~2.16.0B:~2.16.0B:~2.16.0B", [ B1,B2,B3,B4,B5,B6 ]);

unpack(version, Packet, Offset) -> 
    <<_:Offset/binary,B:2/binary,_/binary>> = Packet,
    <<Major,Minor>> = B,
    io_lib:format("~2.16.0B~2.16.0B", [ Major, Minor ]);

unpack(date, Packet, Offset) ->
    <<_:Offset/binary,B:4/binary,_/binary>> = Packet,
    <<YYYY:2/binary,MM:1/binary,DD:1/binary>> = B,
    Year  = bcd_to_integer(YYYY),
    Month = bcd_to_integer(MM),
    Day = bcd_to_integer(DD),
    { Year, Month, Day };

unpack(shortdate, Packet, Offset) ->
    <<_:Offset/binary,B:3/binary,_/binary>> = Packet,
    <<YY:1/binary,MM:1/binary, DD:1/binary>> = B,

    Year  = bcd_to_integer(YY),
    Month = bcd_to_integer(MM),
    Day = bcd_to_integer(DD),

    { 2000 + Year, Month, Day };

unpack(time, Packet, Offset) ->
    <<_:Offset/binary,B:3/binary,_/binary>> = Packet,
    <<HH:1/binary,Mm:1/binary,SS:1/binary>> = B,

    Hour = bcd_to_integer(HH),
    Minute = bcd_to_integer(Mm),
    Second = bcd_to_integer(SS),

    { Hour, Minute, Second };

unpack(datetime, Packet, Offset) ->
    <<_:Offset/binary,B:7/binary,_/binary>> = Packet,
    <<YYYY:2/binary,MM:1/binary,DD:1/binary,HH:1/binary,Mm:1/binary,SS:1/binary>> = B,

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
