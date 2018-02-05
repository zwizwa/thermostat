-module(thermostat).
-export([start/0,
         network/0, icepoint/0,
         set_furnace_state/1,
         get_furnace_state/0,
         %% Internal late binding
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
                 target => {lroom, 17.0},
                 spread => 0.5,
                 
                 %% Infrastructure
                 socket => Socket, 
                 log => Log,
                 ping => serv:start({body, fun() -> thermostat:ping(Pid) end})
                },
               log(State, {start, calendar:local_time()}),
               log(State, {furnace, get_furnace_state()}),
               log(State, {network, network()}),
               log(State, {icepoint, icepoint()}),
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
handle({udp, Socket, {10,1,3,ID}, _, 
        <<255,255,                  %% Magic
          DAC:16/little-signed>>},  %% 8.8 fixed point celcius
       #{socket := Socket} = State) ->

    case maps:find(ID, network()) of
        error ->
            log(State,{unknown, ID}),
            io:format("unknown: ~p~n",[ID]),
            State;
        {ok, {Name, _Desc}} ->
            DAC0 = maps:get(Name, icepoint(), 0.0),
            log(State,{dac,Name,DAC}),
            Temp = (DAC - DAC0) / 256.0,
            self() ! {temp, Name, Temp},
            State
    end;

%% Debug case for bad UDP.
handle({udp, _, _, _, _}=Msg, State) ->
    io:format("ignoring: ~p~n",[Msg]), State;

%% Generic temperature input.
handle({temp, Name, Temp},
       State = #{target := {Current, _Setpoint}}) ->

    log(State,{temp,Name,Temp}),
    
    %% Regulator update if device is current.
    State0 =
        case Name of
            Current -> update(Temp, get_furnace_state(), State);
            _  -> State
        end,
    
    %% Record
    Now = timestamp(),
    maps:merge(State0,
               #{ last => Now,
                  {dev, Name} => {Now, Temp} });

%% Check if current thermometer is alive.
handle({Pid, ping}, State = #{ target := {Current, _} }) ->
    obj:reply(Pid,ok),
    Now = timestamp(),
    Check =
        case maps:find({dev, Current}, State) of
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
            io:format("ping: ~p~n",[{Current,E}]),
            log(State, {ping, Current, E}),
            log(State, {furnace, off}),
            set_furnace_state(off)
    end,
    State;

handle({set_target,Current,Setpoint}=Msg, State)
  when is_atom(Current) and is_number(Setpoint) ->
    log(State, Msg),
    maps:merge(State,
               #{ target => {Current, Setpoint} });

handle({set_spread,Spread}=Msg, State) ->
    log(State, Msg),
    maps:merge(State,
               #{ spread => Spread });

handle({Pid,report},State = #{target := {Current, Setpoint}}) ->
    Descs = maps:from_list(maps:values(network())),
    Temps =
        lists:append(
          lists:map(
            fun({{dev,Name},{_,T}}) ->
                    Desc = maps:get(Name,Descs,<<"unknown">>),
                    [{Name,Desc,T,c2f(T)}];
               (_) -> []
            end,
            maps:to_list(State))),
    obj:reply(Pid, 
              #{ target  => {Current,Setpoint,c2f(Setpoint)},
                 furnace => get_furnace_state(),
                 temps   => Temps }),
    State;

handle(Msg,State) ->
    obj:handle(Msg,State).
    
c2f(T) -> T*1.8+32.

%% Regulator update
update(T, on=Old,
       #{ spread := Spread,
          target := {_Current, Setpoint}} = State) ->
    New = case T > Setpoint + Spread/2 of
              true  -> off;
              false -> on
          end,
    transition(State, T, Old, New);

update(T, off=Old,
       #{ spread := Spread,
          target := {_Current, Setpoint}} = State) ->
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
icepoint() ->
    #{
       zoo        => 592, 
       lroom      => 192,
       broom      => 160,
       zoe        => 528,
       garage     => 656,
       groom      => 944,
       beaglebone => 336
     }.
