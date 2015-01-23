-module(message_mocker_app).
-compile([{parse_transform, lager_transform}]).
-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    io:format("Hello,Started ~n"),
    Pid = spawn(fun() -> erlang:apply(message_mocker,start_link,_StartArgs) end),
    {ok, Pid}.

%%    message_mocker_sup:start_link().

stop(_State) ->
    ok.
