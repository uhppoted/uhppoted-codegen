-module(log).

-export([debugf/2, infof/2, warnf/2, errorf/2]).

debugf(Tag, Msg) ->
    io:format("DEBUG  ~-8.s  ~p~n", [Tag, Msg]).

infof(Tag, Msg) ->
    io:format("INFO   ~-8.s  ~p~n", [Tag, Msg]).

warnf(Tag, Msg) ->
    io:format("WARN   ~-8.s  ~p~n", [Tag, Msg]).

errorf(Tag, Error) ->
    io:format("ERROR  ~-8.s  ~p~n", [Tag, Error]).