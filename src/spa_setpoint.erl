-module(spa_setpoint).
-export([start/1]).

start(_) ->
    spa:resp(
      #{ title => "Main",
         body => body(),
         ws_start =>
             fun() ->
                     tools:info("spa_setpoint: ~p~n", [self()]),
                     {ok, #{}}
             end
       }).

body() ->
    [{pre,[],"Hello"}].
