-module(message_mocker_app).
-compile([{parse_transform, lager_transform}]).
-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    %%    message_mocker_sup:start_link().
    Pid = spawn_link(fun() -> apply(message_mocker, start, _StartArgs) end),
    {ok, Pid}.

stop(_State) ->
    ok.