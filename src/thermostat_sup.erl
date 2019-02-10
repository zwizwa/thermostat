-module(thermostat_sup).
-behaviour(supervisor).
-export([start_link/0, init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% Do not start thermostat daemon.  Let top level applicaiton do that.
init([]) ->
    {ok, {{one_for_one, 5, 5}, []}}.

