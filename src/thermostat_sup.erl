-module(thermostat_sup).
-behaviour(supervisor).
-export([start_link/0, init/1, serv_start_registered/2]).

start_link() ->
    start_cowboy(),
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_cowboy() ->
    Dispatch = cowboy_router:compile(
                 [{'_', [{"/static/[...]", cowboy_static, {priv_dir, thermostat, "static"}}
                        ,{"/ws",           ws, []   }      %% Websocket
                        ,{"/[...]",        ui_thermostat, []}  %% Catch-all dispatcher
                        ]}]),
    NbAcceptors = 1, %% Not performance-criticial
    _ = cowboy:start_http(http, NbAcceptors, [{port, 80}],
                          [{env, [{dispatch, Dispatch}]}]),
    %% Pid is never needed, so don't propagate.
    ok.
    

init([]) ->
    {ok,
     {{one_for_one, 5, 5},
      [serv_worker(thermostat, {spawner, fun thermostat:start/0})]}}.


%% Default record for permanent task.
worker(Name,Mod,Fun,Args) ->
    {Name,{Mod,Fun,Args},permanent,brutal_kill,worker,[Mod]}.
%% Use starter mechanism from serv.erl
serv_start_registered(Name, Spawner) ->
    {ok, serv:up(Name, Spawner)}.
serv_worker(Name,Spawner) ->
    worker(Name,?MODULE,serv_start_registered,[Name,Spawner]).
