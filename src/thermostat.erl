-module(thermostat).
-export([start/0, handle/2]).

start() ->
    serv:start(
      {handler,
       fun () ->
               Pid = self(), 
               {ok, Socket} = gen_udp:open(2001,[binary]),
               #{
                  dev => 24, %% broom
                  socket => Socket, 
                  ping => serv:start({periodic, 60000, fun() -> Pid ! ping end})
                }
       end, 
       fun thermostat:handle/2}).

timestamp() ->
    erlang:monotonic_time(second).

handle({udp, Socket, {10,1,3,From}, _, 
        <<255,255,                  %% Ad-hoc type marker
          DAC:16/little-signed>>},  %% 8.8 fixed point celcius
       #{socket := Socket} = State) ->
    Now = timestamp(),
    Temp = DAC / 256.0,
    %%io:format("temp: ~p~n",[{From,Temp}]),
    maps:merge(State,
               #{ last => Now,
                  {dev, From} => {Now, Temp} });

handle({udp, _, _, _, _}=Msg, State) ->
    io:format("ignoring: ~p~n",[Msg]), State;

handle(ping, State = #{ dev := Dev }) ->
    Now = timestamp(),
    case maps:find({dev, Dev}, State) of
        {ok, {Time, _Temp}} ->
            io:format("ping: dT = ~p~n", [Now - Time]);
        _ ->
            io:format("ping: ~p not active~n", [Dev])
    end,
    State;

handle({dev,Dev}, State) ->
    maps:put(dev, Dev, State);

handle(Msg,State) ->
    obj:handle(Msg,State).
    


%% Watchdog: sends a message every minute.  This is used by the server
%% to shut down the furnace if no readings came in.




