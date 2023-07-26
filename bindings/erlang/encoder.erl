-module(encoder).

-export([ 
    {{- template "export" (index .model.requests 0) -}},
    {{- template "export" (index .model.requests 1) -}},
    {{- template "export" (index .model.requests 2) -}},
    {{- template "export" (index .model.requests 3) -}},
    {{- template "export" (index .model.requests 5)}}
]).

{{ template "request" (index .model.requests 0)}}
{{ template "request" (index .model.requests 1)}}
{{ template "request" (index .model.requests 2)}}
{{ template "request" (index .model.requests 3)}}
{{ template "request" (index .model.requests 5)}}

{{- define "export"}}
    {{snakeCase .name}}/{{count .fields -}}
{{end}}

{{define "request"}}
{{snakeCase .name}}({{template "args" .fields}}) ->
    P = <<16#17, 16#{{ slice (byte2hex .msgtype) 2}}, 16#00, 16#00, 0:60/little-unsigned-integer-unit:8>>,

    Fields = [
    {{- range .fields -}}
    {{- if ne .type "magic"}}
       { {{snakeCase .type}}, {{CamelCase .name}}, {{.offset}} },
    {{- else}}
       { uint32, 16#55aaaa55, {{.offset}} },
    {{- end -}}
    {{- end}}
       { eof, none, none }
    ],

    pack(P,Fields).
{{- end}}

pack(Packet, Fields) ->
    lists:foldl(fun({T, V, Offset},P) -> pack(T, V, P, Offset) end, Packet, Fields).

pack(uint32, V, Packet, Offset) ->
    B = pad(binary:encode_unsigned(V,little), 4),
    <<P:Offset/binary,_:32,R/binary>> = Packet,
    <<P/binary,B/binary,R/binary>>;

pack(ipv4, {B1,B2,B3,B4}, Packet, Offset) ->
    B1x = binary:encode_unsigned(B1,little),
    B2x = binary:encode_unsigned(B2,little),
    B3x = binary:encode_unsigned(B3,little),
    B4x = binary:encode_unsigned(B4,little),
    <<P:Offset/binary,_:32,R/binary>> = Packet,
    <<P/binary,B1x:1/binary,B2x:1/binary,B3x:1/binary,B4x:1/binary,R/binary>>;

pack(datetime, { {Year, Month, Day}, {Hour, Minute, Second} }, Packet, Offset) ->
    YYYYMMDDHHmmss = io_lib:format("~4..0B~2..0B~2..0B~2..0B~2..0B~2..0B",[Year, Month, Day, Hour, Minute, Second]),
    B = string2bcd(YYYYMMDDHHmmss),
    <<P:Offset/binary,_:7/binary,R/binary>> = Packet,
    <<P/binary,B/binary,R/binary>>;

pack(eof,_,Packet,_) ->
    Packet.

pad(B, N) ->
    Padding = 8*(N - byte_size(B)),
    << <<0:Padding>>/binary, B/binary>>.

string2bcd(S) ->
    V = lists:foldl(fun(D,A) -> [bcd(D) | A] end, [], S),
    lists:foldl(fun(D,A) -> <<D:4,A/bitstring>> end, <<>>, V).

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
