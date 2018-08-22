%%%-------------------------------------------------------------------
%%
%% Command receiver process registry
%%
%%%-------------------------------------------------------------------

-module(dispatcher_registry).

%% API
-export([register/2, unregister/2, get_pid/1, get_all_pids/1]).

%%====================================================================
%% API functions
%%====================================================================

-spec register(atom(), pid()) -> ok.
register(Group, Pid) ->
    pg2:create(Group),
    pg2:join(Group, Pid),
    lager:info("register pid ~p in group ~p", [Pid, Group]),
    ok.

-spec unregister(atom(), pid()) -> ok.
unregister(Group, Pid) ->
    pg2:leave(Group, Pid),
    lager:info("unregister pid ~p in group ~p", [Pid, Group]),
    ok.

-spec get_pid(atom()) -> pid() | {error, empty_process_group}.
get_pid(Group) ->
    Members = pg2:get_members(Group),
    lager:info("members: ~p", [Members]),
    Members1 = lists:map(
      fun(Pid) ->
          [{message_queue_len, Messages}] = erlang:process_info(Pid, [message_queue_len]),
          {Pid, Messages}
      end, Members),
    case lists:keysort(2, Members1) of
        [{Pid, _} | _] -> Pid;
        [] -> {error, empty_process_group}
    end.

-spec get_all_pids(atom()) -> [pid()] | {error, empty_process_group}.
get_all_pids(Group) ->
    pg2:get_members(Group).