-module(thermostat).
-export([start/0, handle/2]).

start() ->
    serv:start({handler,
                fun ()-> #{ port => gen_udp:open(2001) } end, 
                fun thermostat:handle/2}).

handle({udp, Socket, FromIP, _FromPort, DAC},
       #{port := {ok, Socket}} = State) ->
    io:format("temp: ~p~n",[{FromIP,DAC}]),
    State;
handle(Msg,State) ->
    obj:handle(Msg,State).
    




