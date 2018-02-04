-module(thermostat).
-export([start/0,
         network/0, icepoint/0,
         set_furnace_state/1,
         get_furnace_state/0,
         %% Internal
         handle/2, ping/1]).

%% Calibration: keep the devices associated to IP addresses, and
%% always log non-calibrated values.

start() ->
    serv:start(
      {handler,
       fun () ->
               Pid = self(), 
               {ok, Socket} = gen_udp:open(2001,[binary]),
               {ok, Log} = file:open("/var/log/thermostat", [append, delayed_write]),
               State = #{
                 %% Controller Config
                 dev => 23, %% lroom
                 setpoint => 18.5,
                 spread => 0.5,
                 icepoint => icepoint(),
                 
                 %% Infrastructure
                 socket => Socket, 
                 log => Log,
                 ping => serv:start({body, fun() -> thermostat:ping(Pid) end})
                },
               log(State, {start, calendar:local_time()}),
               log(State, {furnace, get_furnace_state()}),
               State
       end, 
       fun thermostat:handle/2}).

%% Watchdog: sends a message every minute.  This is used by the server
%% to shut down the furnace if no readings came in.
ping(Pid) ->
    timer:sleep(60000),
    ok = obj:call(Pid, ping),
    thermostat:ping(Pid).

timestamp() ->
    erlang:monotonic_time(second).

%% Thermometer input.
handle({udp, Socket, {10,1,3,From}, _, 
        <<255,255,                  %% Magic
          DAC:16/little-signed>>},  %% 8.8 fixed point celcius
       #{socket := Socket, dev := Dev, icepoint := Icepoint} = State) ->
    Now = timestamp(),
    DAC0 = maps:get(From, Icepoint, 0.0),
    Temp = (DAC - DAC0) / 256.0,
    %%io:format("temp: ~p~n",[{From,Temp}]),

    %% Log both uncalibrated raw DAC value, and calibrated Celcius
    %% temperature.
    log(State,{temp,From,DAC,Temp}),

    %% Regulator update if device is current.
    State0 =
        case From of
            Dev -> update(Temp, get_furnace_state(), State);
            _   -> State
        end,

    %% Record
    maps:merge(State0,
               #{ last => Now,
                  {dev, From} => {Now, Temp} });

%% Debug case for bad UDP.
handle({udp, _, _, _, _}=Msg, State) ->
    io:format("ignoring: ~p~n",[Msg]), State;

%% Check if current thermometer is alive.
handle({Pid, ping}, State = #{ dev := Dev }) ->
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
            log(State, {ping, Dev, E}),
            log(State, {furnace, off}),
            set_furnace_state(off)
    end,
    State;

handle({set_target,Dev,Setpoint}=Msg, State) ->
    log(State, Msg),
    maps:merge(State,
               #{ dev => Dev,
                  setpoint => Setpoint });

handle({set_spread,Spread}=Msg, State) ->
    log(State, Msg),
    maps:merge(State,
               #{ spread => Spread });

handle({set_calib,Calib}=Msg, State) ->
    log(State, Msg),
    maps:merge(State,
               #{ calib => Calib});

handle({Pid,report},State) ->
    Network = network(),
    Report =
        lists:append(
          lists:map(
            fun({{dev,Tag},{_,T}}) ->
                    {Name,Desc} = maps:get(Tag,Network,{unknown,<<"unknown">>}),
                    [{Tag,Name,Desc,T,T*1.8+32}];
               (_) -> []
            end,
            maps:to_list(State))),
    obj:reply(Pid, Report),
    State;

handle(Msg,State) ->
    obj:handle(Msg,State).
    

%% Regulator update
update(T, on=Old,
       #{ spread   := Spread,
          setpoint := Setpoint} = State) ->
    New = case T > Setpoint + Spread/2 of
              true  -> off;
              false -> on
          end,
    transition(State, T, Old, New);

update(T, off=Old,
       #{ spread   := Spread,
          setpoint := Setpoint} = State) ->
    New = case T =< Setpoint - Spread/2 of
              true  -> on;
              false -> off
          end,
    transition(State, T, Old, New).

%% Furnace control
transition(State, _, Old, Old) ->
    State;
transition(State, T, Old, New) ->
    log(State, {furnace, New}),
    set_furnace_state(New),
    io:format("~p: ~p -> ~p~n",[T, Old,New]),
    State.

%% Always use actual pin state.  Don't duplicate states.
get_furnace_state() ->
    case os:cmd("/usr/local/bin/furnace.sh value") of
        "1\n" -> off;
        "0\n" -> on
    end.
set_furnace_state(on)  -> os:cmd("/usr/local/bin/furnace.sh on");
set_furnace_state(off) -> os:cmd("/usr/local/bin/furnace.sh off").

log(#{log := File}, Term) ->
    file:write(File, io_lib:format("~p~n",[{timestamp(),Term}])).


%% hostno -> {host, calib, desc}.
network() -> 
    #{
       19 => {groom,      <<"Guest Room">>},  %% zora on docking station
       18 => {garage,     <<"Garage">>},      %% pi3
       2  => {zoo,        <<"Dining Room">>},
       12 => {zoe,        <<"Tom Office">>},
       23 => {lroom,      <<"Living Room">>},
       24 => {broom,      <<"Bedroom">>},
       97 => {beaglebone, <<"Basement">>}
     }.

%% ICE point DAC values
icepoint() -> #{
       2 => 592, %% zoo
       23 => 192, %% lroom
       24 => 160, %% broom
       12 => 528, %% zoe
       18 => 656, %% garage/pi3
       19 => 944, %% groom/zora
       97 => 336  %% beaglebone
      }.

      
%% calib() ->
%%     %% Honeywell thermostat as reference.
%%     %% Host => [{Ref,Temp}].
%%     %% Data collected as farenheit.
%%     %% Identified by hostname.
    
%%     CalF = #{
%%       18 => [{56,62.7},{56,62.8},{59,66.3},{62,68.7}],
%%       24 => [{56,58.1},{56,58.5},{59,62.9},{62,65.0}],
%%       97 => [{56,60.1},{56,60.6},{59,62.2},{62,65.9}],
%%       23 => [{56,58.0},{56,58.7},{59,62.0},{62,63.8}],
%%       12 => [{56,60.8},{56,61.2},{59,64.1},{62,66.3}],
%%       2  => [{56,59.8},{56,60.1},{59,63.6},{62,65.6}],
%%       19 => [{59,67.6},{62,69.2}]  %% far off, possibly bad data, was broken slot7
%%      },
%%     maps:map(
%%       fun(_Name, TableF) ->
%%               %% [{Ref,Temp}] -> Avg
%%               SumDiffF = lists:foldl(
%%                            fun({RefF,TempF},Acc) -> Acc + RefF - TempF end,
%%                            0, TableF),
%%               CorrF = SumDiffF / length(TableF),
%%               CorrC = CorrF / 1.8,
%%               %% Same resolution as sensor.
%%               round(CorrC,16)
%%       end,
%%       CalF).

%% round(C,N) -> round(C * N) / N.
        

