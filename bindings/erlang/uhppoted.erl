-module(uhppoted).

-export([ get_all_controllers/1 ]).

get_all_controllers (_Config) ->
    Request = encoder:get_controller_request(405419896),
    Bytes = byte_size(Request),
    io:fwrite("exec::~p ~p~n~p~n", [ get_all_controllers, Bytes, Request ]),
    [].

   % request, err := GetControllerRequest(0)
   %  if err != nil {
   %      return nil, err
   %  }

   %  replies, err := broadcast(request)
   %  if err != nil {
   %      return nil, err
   %  }

   %  list := []*GetControllerResponse{}
   %  for _, reply := range replies {
   %      if response, err := getControllerResponse(reply); err != nil {
   %          return nil, err
   %      } else if response != nil {
   %          list = append(list, response)
   %      }
   %  }

   %  return list, nil