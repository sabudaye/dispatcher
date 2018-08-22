%% -------------------------------------------------------------------
%%
%% Command dispatcher Application
%%
%% -------------------------------------------------------------------
-module(dispatcher_app).

-behaviour(application).

%% Application callbacks export
-export([start/2,
         stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================
-spec start(any(), list()) -> {ok, pid()} | ignore | {error, atom()}.
start(_StartType, _StartArgs) ->
    dispatcher_sup:start_link().

-spec stop(any()) -> ok.
stop(_State) ->
    ok.
