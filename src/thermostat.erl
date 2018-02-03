-module(thermostat).
-export([start/0,
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
       #{socket := Socket, dev := Dev} = State) ->
    Now = timestamp(),
    Temp = DAC / 256.0,
    %%io:format("temp: ~p~n",[{From,Temp}]),

    log(State,{temp,From,Temp}),

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
    transition(State, Old, New);

update(T, off=Old,
       #{ spread   := Spread,
          setpoint := Setpoint} = State) ->
    New = case T < Setpoint - Spread/2 of
              true  -> on;
              false -> off
          end,
    transition(State, Old, New).

%% Furnace control
transition(State, Old, Old) ->
    State;
transition(State, Old, New) ->
    log(State, {furnace, New}),
    set_furnace_state(New),
    io:format("~p -> ~p~n",[Old,New]),
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
