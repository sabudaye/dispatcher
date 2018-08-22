%%%-------------------------------------------------------------------
%%
%% Helper module for working with dispatcher rulesets
%%
%%%-------------------------------------------------------------------

-module(dispatcher_rulesets).

-export([new/0, load_rulesets/1, get/0, get/1, get/2, set/1, set/2]).

-define(TABLE_NAME, dispatcher_rulesets).

-include("dispatcher.hrl").

-spec new() -> ok.
new() ->
    %% set up the ETS configuration table
    _ = try ets:new(?TABLE_NAME, [named_table, public, set, {keypos, 1}, {read_concurrency, true}]) of
        _Result ->
            lager:warning("rulesets table created ~p", [?TABLE_NAME]),
            ok
    catch
        error:badarg ->
            lager:warning("Rulesets table ~p already exists", [?TABLE_NAME])
    end,
    Rulesets = dispatcher_config:rulesets(),
    load_rulesets(Rulesets),
    ok.

-spec load_rulesets(list()) -> ok.
load_rulesets(Rulesets) ->
    case Rulesets of
      [] ->
        lager:info("empty list of rulesets");
      _ ->
        lager:info("loading rulesets: ~p", [Rulesets]),
        lists:map(fun set/1, Rulesets),
        lager:info("rulesets loaded", [])
    end,
    ok.

-spec get() -> list().
get() ->
    get(?DEFAULT_GROUPNAME, []).

-spec get(atom()) -> list().
get(GroupName) ->
    get(GroupName, []).

-spec get(atom(), term()) -> list().
get(GroupName, Default) ->
    try
    case ets:lookup(?TABLE_NAME, GroupName) of
        [] ->
            Default;
        [{GroupName, Res}] ->
            Res
    end
    catch
        _:_ ->
            Default
    end.

-spec set(tuple() | term()) -> true.
set({GroupName, Value}) ->
    set(GroupName, Value);
set(Value) ->
    set(?DEFAULT_GROUPNAME, Value).

-spec set(atom(), term()) -> true.
set(GroupName, Value) ->
    ets:insert(?TABLE_NAME, {GroupName, Value}).