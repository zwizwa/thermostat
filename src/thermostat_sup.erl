-module(thermostat_sup).
-behaviour(supervisor).
-export([start_link/0, init/1, serv_start_registered/2]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    case filelib:is_file("/usr/local/bin/furnace.sh") of
        true ->
            {ok,
             {{one_for_one, 5, 5},
              [serv_worker(thermostat, {spawner, fun thermostat:start/0})]}};
        false ->
            {ok, {{one_for_one, 5, 5}, []}}
    end.

%% Default record for permanent task.
worker(Name,Mod,Fun,Args) ->
    {Name,{Mod,Fun,Args},permanent,brutal_kill,worker,[Mod]}.
%% Use starter mechanism from serv.erl
serv_start_registered(Name, Spawner) ->
    {ok, serv:up(Name, Spawner)}.
serv_worker(Name,Spawner) ->
    worker(Name,?MODULE,serv_start_registered,[Name,Spawner]).
