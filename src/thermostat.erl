-module(thermostat).
-export([start_link/0,
         network/0, icepoint/0,
         set_furnace_state/1,
         get_furnace_state/0,
         temperv14_start/2,
         %% Internal late binding
         recorder/0,
         temperv14_handle/2,
         handle/2, ping/1, snapshot/1]).

%% Calibration: keep the devices associated to IP addresses, and
%% always log non-calibrated values.

-type state() :: 
        #{ target := {atom(), number()},
           spread := number(),
           socket := port(),
           log    := pid(),
           ping   := pid() }.
                    
start_link() ->
    Pid = start(),
    register(thermostat, Pid), 
    {ok, Pid}.
start() ->
    serv:start(
      {handler,
       fun () ->
               Pid = self(), 
               {ok, Socket} = gen_udp:open(2001,[binary]),
               %% {ok, Log} = file:open("/var/log/thermostat", [append, delayed_write]),
               State = #{
                 %% Controller Config
                 target => {lroom, undefined},
                 spread => 0.5,
                 
                 %% Infrastructure
                 socket => Socket, 
                 %% log => Log,
                 recorder => recorder(),
                 bc => serv:bc_start(),
                 ping => serv:start({body, fun() -> ?MODULE:ping(Pid) end})
                },
               notify(State, {start, calendar:local_time()}),
               notify(State, {furnace, get_furnace_state()}),
               notify(State, {network, network()}),
               notify(State, {icepoint, icepoint()}),
               State
       end, 
       fun ?MODULE:handle/2}).

%% Watchdog: sends a message every minute.  This is used by the server
%% to shut down the furnace if no readings came in for the target
%% sensor.
ping(Pid) ->
    timer:sleep(60000),
    ok = obj:call(Pid, ping),
    ?MODULE:ping(Pid).

-spec timestamp() -> integer().
timestamp() ->
    erlang:monotonic_time(second).

%% Thermometer input.

%% Low level UDP messages.
handle({udp, Socket, {10,1,3,ID}, _, 
        <<255,255,                  %% Magic
          ADC:16/little-signed>>},  %% 8.8 fixed point celcius
       #{socket := Socket} = State) when is_number(ID) ->
    handle({sensor, ID, ADC}, State);

%% Debug case for bad UDP.
handle({udp, _, _, _, _}=Msg, State) ->
    io:format("ignoring: ~p~n",[Msg]), State;

%% High level term interface.  ADC values are still raw, so apply
%% calibration data here.
handle({sensor, ID, ADC}=_Msg, State) when is_number(ID) ->
    case maps:find(ID, ?MODULE:network()) of
        error ->
            notify(State,{unknown, ID}),
            io:format("unknown: ~p~n",[ID]),
            State;
        {ok, {Name, _Desc}} ->
            ADC0 = maps:get(Name, icepoint(), 0.0),
            notify(State,{dac,Name,ADC}),
            Temp = (ADC - ADC0) / 256.0,
            self() ! {temp, Name, Temp},
            State
    end;

%% Measurement input with setpoint inactive: pick the first
%% measurement, so no action is taken at startup.
handle({temp, Name, Temp}=_Msg,
       State = #{target := {Current, undefined}}) ->
    %% log:info("1: ~p~n",[_Msg]),
    case Name of
        Current ->
            NewTarget = {Current, Temp},
            io:format("target: ~p~n", [NewTarget]),
            maps:put(target, NewTarget, State);
        _ ->
            State
    end;

%% Measurement input with setpoint active.
handle({temp, Name, Temp}=_Msg,
       State = #{target := {Current, _Setpoint}}) ->

    %% log:info("2: ~p~n",[_Msg]),
    notify(State,{temp,Name,Temp}),
    
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

%% Perform a sensor check.
handle({Pid, ping}, State = #{ target := {Current, _} }) ->
    obj:reply(Pid,ok),
    Now = timestamp(),
    TimeoutSec = 60,

    Deltas  = [{D, Now - Last} || {{dev,D},{Last,_Temp}} <- maps:to_list(State)],
    %% log:info("ping: deltas: ~p~n", [maps:from_list(Deltas)]),
    lists:foldl(
      fun({Name, Delta}, S) ->
              case Delta > TimeoutSec of
                  false ->
                      %% Sensor is ok
                      S;
                  true  ->
                      %% Sensor is bad
                      %% Propagate to all listeners (e.g. web gui)
                      notify(State, {remove, Name, {timeout_sec, TimeoutSec}}),
                      %% As a safety measure, switch off the furnace
                      %% if the current sensor is not active.  This
                      %% will lead to a task crash.
                      case Name of
                          Current ->
                              notify(State, {furnace, off}),
                              set_furnace_state(off),
                              shutdown;
                          _ ->
                              not_current
                      end,
                      %% Forget about it.
                      maps:remove({dev, Name}, State)
              end
      end, 
      State,
      Deltas);


handle({set_target,Current,Setpoint}=Msg, State)
  when is_atom(Current) and is_number(Setpoint) ->
    notify(State, Msg),
    maps:merge(State,
               #{ target => {Current, Setpoint} });

handle({set_spread,Spread}=Msg, State) ->
    notify(State, Msg),
    maps:merge(State,
               #{ spread => Spread });

%% handle({Pid,report},State = #{target := {Current, Setpoint}}) ->
%%     Descs = maps:from_list(maps:values(network())),
%%     Temps =
%%         lists:append(
%%           lists:map(
%%             fun({{dev,Name},{_,T}}) ->
%%                     Desc = maps:get(Name,Descs,<<"unknown">>),
%%                     [{Name,Desc,T,c2f(T)}];
%%                (_) -> []
%%             end,
%%             maps:to_list(State))),
%%     obj:reply(Pid, 
%%               #{ target  => {Current,Setpoint,c2f(Setpoint)},
%%                  furnace => get_furnace_state(),
%%                  temps   => Temps }),
%%     State;

handle({Pid,snapshot},State) ->
    %% Remove internal bits.
    Snapshot =
        maps:merge(
          %% Remove internal bits
          lists:foldl(
            fun(F,S) -> F(S) end, State,
            [fun(M) -> maps:remove(K,M) end || K <- [socket, ping, recorder]]),
          %% This has to run on the actuator host, which is the main
          %% reason why snapshot is implemented as rpc method.
          #{ furnace => get_furnace_state() }),

    %% Record furnace state
    obj:reply(Pid, Snapshot),
    State;


handle(Msg,State) ->
    obj:handle(Msg,State).

%% -spec c2f(number()) -> number().    
%% c2f(T) -> T*1.8+32.

%% Regulator update
-spec update(number(), on|off, state()) -> state().
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
    notify(State, {furnace, New}),
    set_furnace_state(New),
    io:format("~p: ~p -> ~p~n",[T, Old,New]),
    State.

%% Always use actual pin state.  Don't duplicate states.
get_furnace_state() ->
    case os:cmd("/usr/local/bin/furnace.sh state") of
        "off\n" -> off;
        "on\n" -> on
    end.
set_furnace_state(on)  -> os:cmd("/usr/local/bin/furnace.sh on");
set_furnace_state(off) -> os:cmd("/usr/local/bin/furnace.sh off").

notify(#{recorder := Recorder, bc := BC}, Term) ->
    BC ! {broadcast, {thermostat, Term }},
    Recorder ! Term,
    %% Old style text file.  Obsolete.
    %%file:write(File, io_lib:format("~p~n",[{timestamp(),Term}])).
    ok.


%% User access
snapshot(Pid) ->
    obj:call(Pid, snapshot).


%% hostno -> {host, calib, desc}.
-spec network() -> #{ number() => {atom(), binary()} }.
network() -> 
    #{
       24 => {groom,      <<"Guest Room">>},  %% nexx0
       98 => {garage,     <<"Garage">>},      %% nexx1
       2  => {zoo,        <<"Dining Room">>},
       12 => {zoe,        <<"Tom Office">>},
       23 => {lroom,      <<"Living Room">>},
       99 => {broom,      <<"Bedroom">>},
       89 => {basement,   <<"Basement">>}
     }.
name_to_id(Name) ->
    Map = maps:from_list([{K,V} || {V,{K,_}} <- maps:to_list(network())]),
    maps:get(Name, Map).

%% ICE point ADC values
-spec icepoint() -> #{ atom() => integer() }.
icepoint() -> #{
          zoo        => 592, 
          lroom      => 192,
          broom      => 160,
          groom      => 944,
          zoe        => 528,
          garage     => 656, %% nexx1
          basement   => 336
      }.

%% Service supervisor for thermometers.

%% What should this do?  This bridges the gaps between what is
%% expected, and what is there.  In essences it is just a supervisor
%% that keeps restarting if a sensor is not there.

%% temperv14 sensors are buggy so they should be run under
%% supervision.  Port programs operate best as query/response, as they
%% should exit on stdin close.

%% FIXME: This needs to be gentler.

%%temperv14_start(N) when is_number(N) ->
%%    temperv14_start(tools:format("10.1.1.~p",[N]));
temperv14_start(Name,DST) when is_atom(Name) ->
    ID = name_to_id(Name),
    Host = tools:format("10.1.1.~p",[ID]),
    temperv14_start({Host,ID},DST);

%% FIXME: monitor the thermostat.
temperv14_start({Host,ID},Thermostat) ->
    serv:start_child(
      {handler,
       fun() ->
               log:set_info_name({temperv14,{Host,ID}}),
               log:info("starting sensor monitor~n",[Host]),
               _Ref = erlang:monitor(process, Thermostat),
               temperv14_handle(
                 connect, 
                 #{ dst  => Thermostat, 
                    host => Host,
                    id   => ID })
       end,
       fun ?MODULE:temperv14_handle/2}).

temperv14_handle(_Msg=connect,
                 #{host := Host}=State) ->
    Port = exo:open_ssh_port(Host, "temperv14", [{line, 1024}]),
    State1 = maps:put(port, Port, State),
    temperv14_handle({start, 5000}, State1);

temperv14_handle({start,Delay}=Msg, State) ->
    self() ! read,
    {ok, TRef} = timer:send_after(Delay, Msg),
    maps:put(tref, TRef, State);

temperv14_handle(read, #{ port := Port }=State) ->
    Port ! {self(), {command, "\n"}},
    State;

temperv14_handle({Port,{data,{eol,Bin}}},
                 #{port:=Port,dst:=Dst,id:=ID}=State) ->
    [Dec|_] = re:split(Bin," "),
    ADC = binary_to_integer(Dec),
    Dst ! {sensor,ID,ADC},
    %% log:info("C=~p~n", [ADC/256.0]),
    State;

temperv14_handle(_Msg={Port,{exit_status,_}},
                 #{port:=Port,id:=_ID}=State) ->
    log:info("temperv14: ~p~n", [{_ID,_Msg}]),
    %% We handle sensor poop-outs here.
    timer:sleep(5000),
    temperv14_handle(connect, State);

temperv14_handle(Msg,State) ->
    %% log:info("~p~n", [Msg]),
    obj:handle(Msg,State).

%% On OpenWRT hotplug:
%% root@nexx1:~# cat /etc/hotplug.d/usb/99-temperv14 
%% [ $ACTION == add ] && [ $PRODUCT == 'c45/7401/1' ] && chown exo:exo /dev/$DEVNAME




recorder() ->
    {ok, Pid} = 
        recorder:start_link(
          #{ dir => "/var/log/thermostat.d/",
             nb_chunks => 400,
             chunk_size => 10*1000*1000 }),
    Pid.
