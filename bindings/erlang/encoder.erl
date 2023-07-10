-module(encoder).

-export([ get_controller_request/1 ]).

get_controller_request (Controller) ->
    << 16#17, 16#94, 16#00, 16#00, 
       Controller:4/little-unsigned-integer-unit:8,
       0:56/little-unsigned-integer-unit:8
    >>.
