-module(encoder).

-export([ 
    {{- range (subslice .model.requests)}}
    {{- template "export" . -}},{{end}}
    {{- range (last .model.requests)}}
    {{- template "export" . -}}{{end}}
]).

{{- range .model.requests}}
{{ template "request" .}}
{{- end}}

{{- define "export"}}
    {{snakeCase .name}}/{{count .fields -}}
{{end}}

{{define "request"}}
{{snakeCase .name}}({{template "args" .fields}}) ->
    P = <<16#17, 16#{{ slice (byte2hex .msgtype) 2}}, 16#00, 16#00, 0:60/little-unsigned-integer-unit:8>>,

    Fields = [
    {{- range .fields -}}
    {{- if ne .type "magic"}}
        { {{- snakeCase .type}}, {{CamelCase .name}}, {{.offset}}},
    {{- else}}
        {uint32, 16#55aaaa55, {{.offset}}},
    {{- end -}}
    {{- end}}
        {eof, none, none}
    ],

    pack(P, Fields).
{{- end}}

pack(Packet, Fields) ->
    lists:foldl(fun({T, V, Offset}, P) -> pack(T, V, P, Offset) end, Packet, Fields).

pack(bool, true, Packet, Offset) ->
    B = binary:encode_unsigned(1, little),
    <<P:Offset/binary, _:8, R/binary>> = Packet,
    <<P/binary, B/binary, R/binary>>;

pack(bool, _, Packet, Offset) ->
    B = binary:encode_unsigned(0, little),
    <<P:Offset/binary, _:8, R/binary>> = Packet,
    <<P/binary, B/binary, R/binary>>;

pack(uint8, V, Packet, Offset) ->
    B = binary:encode_unsigned(V, little),
    <<P:Offset/binary, _:8, R/binary>> = Packet,
    <<P/binary, B/binary, R/binary>>;

pack(uint16, V, Packet, Offset) ->
    B = pad(binary:encode_unsigned(V, little), 2),
    <<P:Offset/binary, _:16, R/binary>> = Packet,
    <<P/binary, B/binary, R/binary>>;

pack(uint32, V, Packet, Offset) ->
    B = pad(binary:encode_unsigned(V, little), 4),
    <<P:Offset/binary, _:32, R/binary>> = Packet,
    <<P/binary, B/binary, R/binary>>;

pack(ipv4, {B1, B2, B3, B4}, Packet, Offset) ->
    B1x = binary:encode_unsigned(B1, little),
    B2x = binary:encode_unsigned(B2, little),
    B3x = binary:encode_unsigned(B3, little),
    B4x = binary:encode_unsigned(B4, little),
    <<P:Offset/binary, _:32, R/binary>> = Packet,
    <<P/binary, B1x:1/binary, B2x:1/binary, B3x:1/binary, B4x:1/binary, R/binary>>;

pack(datetime, { {Year, Month, Day}, {Hour, Minute, Second}}, Packet, Offset) ->
    YYYYMMDDHHmmss = io_lib:format("~4..0B~2..0B~2..0B~2..0B~2..0B~2..0B", [Year, Month, Day, Hour, Minute, Second]),
    B = string2bcd(YYYYMMDDHHmmss),
    <<P:Offset/binary, _:7/binary, R/binary>> = Packet,
    <<P/binary, B/binary, R/binary>>;

pack(date, {Year, Month, Day}, Packet, Offset) ->
    YYYYMMDD = io_lib:format("~4..0B~2..0B~2..0B", [Year, Month, Day]),
    B = string2bcd(YYYYMMDD),
    <<P:Offset/binary, _:4/binary, R/binary>> = Packet,
    <<P/binary, B/binary, R/binary>>;

pack(pin, V, Packet, Offset) ->
    B = pad(binary:encode_unsigned(V, little), 3),
    <<P:Offset/binary, _:24, R/binary>> = Packet,
    <<P/binary, B/binary, R/binary>>;

pack(task, V, Packet, Offset) ->
    B = binary:encode_unsigned(V, little),
    <<P:Offset/binary, _:8, R/binary>> = Packet,
    <<P/binary, B/binary, R/binary>>;

pack(interlock, V, Packet, Offset) ->
    B = binary:encode_unsigned(V, little),
    <<P:Offset/binary, _:8, R/binary>> = Packet,
    <<P/binary, B/binary, R/binary>>;

pack(hhmm, {Hour, Minute}, Packet, Offset) ->
    HHmm = io_lib:format("~2..0B~2..0B", [Hour, Minute]),
    B = string2bcd(HHmm),
    <<P:Offset/binary, _:2/binary, R/binary>> = Packet,
    <<P/binary, B/binary, R/binary>>;

pack(eof, _, Packet, _) ->
    Packet;

pack(T, _, _, _) ->
    {error, {unsupported_type, T}}.

pad(B, N) ->
    Padding = 8 * (N - byte_size(B)),
    <<B/binary, <<0:Padding>>/binary>>.

string2bcd(S) ->
    V = lists:foldl(fun(D, A) -> [bcd(D) | A] end, [], S),
    lists:foldl(fun(D, A) -> <<D:4, A/bitstring>> end, <<>>, V).

bcd($0) -> 0;
bcd($1) -> 1;
bcd($2) -> 2;
bcd($3) -> 3;
bcd($4) -> 4;
bcd($5) -> 5;
bcd($6) -> 6;
bcd($7) -> 7;
bcd($8) -> 8;
bcd($9) -> 9.
