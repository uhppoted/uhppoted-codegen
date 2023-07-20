-module(log).

-export([ debugf/2, infof/2, warnf/2, errorf/2 ]).

debugf(Tag,Msg) ->
  io:format("DEBUG  ~s  ~p~n", [ Tag, Msg ]).

infof(Tag,Msg) ->
  io:format("INFO   ~s  ~p~n", [ Tag, Msg ]).

warnf(Tag,Msg) ->
  io:format("WARN   ~s  ~p~n", [ Tag, Msg ]).

errorf(Tag,Error) ->
  io:format("ERROR  ~s  ~p~n", [ Tag, Error ]).

