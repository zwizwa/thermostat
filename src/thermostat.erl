-module(thermostat).
-export([start/0,
         %% Internal
         handle/2, ping/1]).

start() ->
    serv:start(
      {handler,
       fun () ->
               Pid = self(), 
               {ok, Socket} = gen_udp:open(2001,[binary]),
               #{
                  dev => 24, %% broom
                  socket => Socket, 
                  ping => serv:start({body, fun() -> thermostat:ping(Pid) end})
                }
       end, 
       fun thermostat:handle/2}).

ping(Pid) ->
    timer:sleep(60000),
    ok = obj:call(Pid, ping),
    thermostat:ping(Pid).

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

handle({Pid,ping}, State = #{ dev := Dev }) ->
    obj:reply(Pid,ok),
    Now = timestamp(),
    Check =
        case maps:find({dev, Dev}, State) of
            {ok, {Time, _Temp}} ->
                DT = Now - Time,
                case DT > 60 of 
                    false -> ok;
                    true -> {error, {dt, DT}}
                end;
            _ ->
                {error, not_active}
        end,
    case Check of
        ok -> ok;
        {error, _}=E ->
            io:format("ping: ~p~n",[{Dev,E}]),
            shutdown_FIXME
    end,
    State;

handle({dev,Dev}, State) ->
    maps:put(dev, Dev, State);

handle(Msg,State) ->
    obj:handle(Msg,State).
    


%% Watchdog: sends a message every minute.  This is used by the server
%% to shut down the furnace if no readings came in.




