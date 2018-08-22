-module(dispatcher).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start/0,
         stop/0,
         command/1]).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-spec start() -> ok.
start() ->
    {ok, _} = application:ensure_all_started(dispatcher),
    lager:info("start dispatcher application", []),
    ok.

-spec stop() -> ok.
stop() ->
    lager:info("stop dispatcher application", []),
    ok = application:stop(dispatcher).

-spec command(term()) -> ok.
command(Command) ->
    lager:info("received command ~p", [Command]),
    dispatcher_command_handler:command(Command).