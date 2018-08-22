-module(dispatcher).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start/0,
         stop/0,
         command/2,
         register_receiver_process/2]).

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

-spec command(term(), term()) -> ok.
command(Command, Args) ->
    lager:info("received command ~p with args ~p", [Command, Args]),
    dispatcher_command_handler:command(Command, Args).

-spec register_receiver_process(atom(), pid()) -> ok.
register_receiver_process(GroupName, Pid) ->
    dispatcher_registry:register(GroupName, Pid).