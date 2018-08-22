%%%-------------------------------------------------------------------
%%
%%  Dispatcher command handler module
%%
%%%-------------------------------------------------------------------

-module(dispatcher_command_handler).

-behaviour(gen_server).

%% API
-export([start_link/0]).

-export([command/2]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-include("dispatcher.hrl").

%% ===================================================================
%% API functions
%% ===================================================================

-spec start_link() -> {ok, pid()} | ignore | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec command(term(), any()) -> ok.
command(Command, Args) ->
    gen_server:cast(?MODULE, {command, Command, Args}).

%% ===================================================================
%% gen_server callbacks
%% ===================================================================

init([]) ->
    {ok, #{}}.

handle_call(_Msg, _From, State) ->
    {reply, ok, State}.

handle_cast({command, {GroupName, Command}, Args}, State) ->
    case dispatcher_rulesets:get(GroupName) of
        [] -> lager:info("empty ruleset in group ~p, command: ~p", [GroupName, Command]);
        Rules ->
            apply_command(GroupName, Command, Args, Rules)
    end,
    {noreply, State};
handle_cast({command, Command, Args}, State) ->
    case dispatcher_rulesets:get() of
        [] -> lager:info("empty ruleset in group ~p, command: ~p", [?DEFAULT_GROUPNAME, Command]);
        Rules ->
            apply_command(?DEFAULT_GROUPNAME, Command, Args, Rules)
    end,
    {noreply, State};
handle_cast(Msg, State) ->
    lager:info("unknownn cast message ~p", [Msg]),
    {noreply, State}.

handle_info(_Msg, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ===================================================================
%% internal functions
%% ===================================================================

apply_command(GroupName, Command, Args, Rules) ->
    case proplists:get_value(Command, Rules) of
        undefined -> lager:info("no rules for command ~p in group ~p, rules: ~p", [Command, GroupName, Rules]);
        {module, ModuleName} ->
            lager:info("call function ~p in module ~p with args ~p, rules: ~p", [Command, ModuleName, Args, Rules]),
            ModuleName:Command(Args);
        {message, one} ->
            case dispatcher_registry:get_pid(GroupName) of
                {error, Error} ->
                    lager:warning(
                        "error ~p while sending message {~p, ~p} to pid in group ~p",
                        [Error, Command, Args, GroupName]);
                Pid ->
                    lager:info("send message {~p, ~p} to pid ~p in group ~p", [Command, Args, Pid, GroupName]),
                    gen_server:cast(Pid, {Command, Args})
            end;
        {message, group} ->
            case dispatcher_registry:get_all_pids(GroupName) of
                {error, Error} ->
                    lager:warning(
                        "error ~p while sending message {~p, ~p} to pid in group ~p",
                        [Error, Command, Args, GroupName]);
                Pids ->
                    lager:info("send message {~p, ~p} to pids ~p in group ~p", [Command, Args, Pids, GroupName]),
                    lists:map(fun(Pid) -> gen_server:cast(Pid, {Command, Args}) end, Pids)
            end;
        _ -> lager:warning("unsupported rule", [])
    end,
    ok.
