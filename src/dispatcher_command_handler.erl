%%%-------------------------------------------------------------------
%%
%%  Dispatcher command handler module
%%
%%%-------------------------------------------------------------------

-module(dispatcher_command_handler).

-behaviour(gen_server).

%% API
-export([start_link/0]).

-export([command/1]).

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

-spec command(term()) -> ok.
command(Command) ->
    gen_server:cast(?MODULE, {command, Command}).

%% ===================================================================
%% gen_server callbacks
%% ===================================================================

init([]) ->
    {ok, #{}}.

handle_call(_Msg, _From, State) ->
    {reply, ok, State}.

handle_cast({command, {GroupName, Command}}, State) ->
    case dispatcher_rulesets:get(GroupName) of
        [] -> lager:info("empty ruleset in group ~p, command: ~p", [GroupName, Command]);
        Rules ->
            apply_command(Command, Rules)
    end,
    {noreply, State};
handle_cast({command, Command}, State) ->
    case dispatcher_rulesets:get() of
        [] -> lager:info("empty ruleset in group ~p, command: ~p", [?DEFAULT_GROUPNAME, Command]);
        Rules ->
            apply_command(Command, Rules)
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

apply_command({Function, Args} = Command, Rules) ->
    case proplists:get_value(Function, Rules) of
        undefined -> lager:info("no rules for command ~p, rules: ~p", [Command, Rules]);
        Module ->
            lager:info("call function ~p in module ~p, rules: ~p", [Function, Module, Rules]),
            Module:Function(Args)
    end,
    ok;
apply_command(Command, Rules) ->
    case proplists:get_value(Command, Rules) of
        undefined -> lager:info("no rules for command ~p, rules: ~p", [Command, Rules]);
        Rule ->
            lager:info("apply command ~p by rule ~p, rules: ~p", [Command, Rule, Rules])
    end,
    ok.
