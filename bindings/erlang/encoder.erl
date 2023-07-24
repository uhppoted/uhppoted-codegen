-module(encoder).

-export([ 
    {{- template "export" (index .model.requests 0) -}}
]).

{{ template "request" (index .model.requests 0) -}}

{{- define "export"}}
    {{snakeCase .name}}/1
{{end}}

{{define "request"}}
{{snakeCase .name}}({{template "args" .fields}}) ->
    P = <<16#17, 16#{{ slice (byte2hex .msgtype) 2}}, 16#00, 16#00, 0:60/little-unsigned-integer-unit:8>>,

    Fields = [
    {{- range .fields -}}
    {{if ne .type "magic"}}
       { {{snakeCase .type}}, {{CamelCase .name}}, {{.offset}} },
    {{- else}}
       { uint32, 16#55aaaa55, {{.offset}} },
    }
    {{- end}}
       { eof, none, none }
    ],{{end}}

    pack(P,Fields).
{{- end}}

pack(Packet, Fields) ->
    lists:foldl(fun(F,P) -> packf(P,F) end, Packet, Fields).

packf(P, {uint32, V, Offset}) ->
    pack_uint32(V,P,Offset);

packf(P,{eof,_,_}) ->
    P;

packf(_,{T,_,_}) ->
    error({unknown_type, T}).


pack_uint32(V, Packet, Offset) ->
    B = pad(binary:encode_unsigned(V,little), 4),
    <<P:Offset/binary,_:32,R/binary>> = Packet,
    <<P/binary,B/binary,R/binary>>.


pad(B, N) ->
    Padding = 8*(N - byte_size(B)),
    << <<0:Padding>>/binary, B/binary>>.

