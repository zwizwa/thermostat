%% Catch-all dispatcher for top level links.
%% These are exposed as user interface.

-module(ui_thermostat).
-behavior(cowboy_http_handler).
-export([init/3, handle/2, terminate/3]).

%% -include("web.hrl").

init({tcp, http}, Req, _Opts) ->
    %% log:info("raw: init~n"),
    {ok, Req, no_state}.
handle(Req, State) ->
    %% Cowboy's API is wrapped so we can use simpler data structures
    %% for constructing replies in web.erl
    web:cowboy_http_handle(
      Req, State,
      fun query/2, fun post/2).

terminate(_Reason, _Req, _State) ->
    ok.



%% Web root
query([], Qv) ->
    case db:find_config({spa_main,0}) of
        %% Debug override
        {ok, {term, {Mod,Fun}}} -> apply(Mod,Fun,[Qv]);
        %% Standard web application: cover sheet
        _ -> query([<<"setpoint">>], Qv)
    end;

query([<<"setpoint">>], Qv) -> spa_setpoint:start(with_nav(cover, Qv));

query([<<"favicon.ico">>], _) -> cowboy_wrap:resp_text("FIXME: icon");


query(BinPath, Qv) ->
    Msg = tools:format("unknown query: ~p ~p~n", [BinPath, Qv]),
    log:info("~s", [Msg]),
    cowboy_wrap:resp_text(Msg).

post(BinPath,PostData) ->
    Msg = tools:format("unknown post: ~p ~p~n", [BinPath,PostData]),
    log:info("~s", [Msg]),
    cowboy_wrap:resp_text(Msg).



%% Wrapper for each page.  See gw/src/ui.erl
with_nav(CurrentSelector, Qv) ->
    User = web:user(Qv),
    log:info("request for ~p from user: ~p~n", [CurrentSelector, User]),
    #{
       user => User,
       query => Qv,
       wrap_body => fun(Body) -> Body end
     }.



    

        
    
