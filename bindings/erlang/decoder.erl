-module(decoder).

-export([ get_controller_response/1 ]).

-record(uhppote_controller, { 
    controller,
    address,
    netmask,
    gateway,
    mac,
    version,
    date
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

unpack_IPv4(<<B1,B2,B3,B4>>) ->
    { B1,B2,B3,B4 }.

unpack_MAC(<<B1,B2,B3,B4,B5,B6>>) ->
    io_lib:format("~2.16.0B:~2.16.0B:~2.16.0B:~2.16.0B:~2.16.0B:~2.16.0B", [ B1,B2,B3,B4,B5,B6 ]).

unpack_version(<<Major,Minor>>) -> 
    io_lib:format("~2.16.0B~2.16.0B", [ Major, Minor ]).

unpack_date(<<CC,YY,MM, DD>>) ->
    { Year,_}  = string:to_integer(io_lib:format("~2.16.0B~2.16.0B", [ CC,YY ])),
    { Month, _ } = string:to_integer(io_lib:format("~2.16.0B", [ MM ])),
    { Day, _ } = string:to_integer(io_lib:format("~2.16.0B", [ DD ])),
    { Year, Month, Day }.

