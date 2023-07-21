-module(encoder).

-export([ get_controller_request/1 ]).

get_controller_request (Controller) ->
    P1 = <<16#17, 16#94, 16#00, 16#00, 0:60/little-unsigned-integer-unit:8>>,
    Packet = pack_uint32(Controller, P1, 4),

    Packet.

pack_uint32(V, Packet, Offset) ->
    B = pad(binary:encode_unsigned(V,little), 4),
    <<P:Offset/binary,_:32,R/binary>> = Packet,
    <<P/binary,B/binary,R/binary>>.


pad(B, N) ->
    Padding = 8*(N - byte_size(B)),
    << <<0:Padding>>/binary, B/binary>>.

