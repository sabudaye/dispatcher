-module(dispatcher).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start/0,
         stop/0]).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-spec start() -> 'ok'.
start() ->
    {ok, _} = application:ensure_all_started(dispatcher),
    ok.

-spec stop() -> 'ok'.
stop() ->
    ok = application:stop(dispatcher).