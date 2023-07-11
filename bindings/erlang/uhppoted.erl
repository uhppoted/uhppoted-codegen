-module(uhppoted).

-export([ get_all_controllers/1 ]).

get_all_controllers (Config) ->
    Request = encoder:get_controller_request(0),
    Replies = udp:broadcast(Config, Request),

    io:fwrite("exec::~p~n~p~n", [ get_all_controllers, Replies ]),

   %  list := []*GetControllerResponse{}
   %  for _, reply := range replies {
   %      if response, err := getControllerResponse(reply); err != nil {
   %          return nil, err
   %      } else if response != nil {
   %          list = append(list, response)
   %      }
   %  }

    [].
