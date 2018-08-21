%% -------------------------------------------------------------------
%%
%% Command dispatcher test suite
%%
%% -------------------------------------------------------------------
-module(dispatcher_SUITE).

%% common_test exports
-export(
   [
    all/0, group/1, groups/0,
    init_per_suite/1, end_per_suite/1,
    init_per_group/2,
    end_per_group/2,
    init_per_testcase/2, end_per_testcase/2
   ]).

%% test case exports
-export(
   [
    test_run/1
   ]).

-include_lib("common_test/include/ct.hrl").

%%%===================================================================
%%% common_test API
%%%===================================================================

all() ->
    [
     {group, main}
    ].

group(main) ->
    [].

groups() ->
    [
     {main, [shuffle],
      [
        test_run
      ]}
    ].

init_per_suite(Config) ->
  Config.

end_per_suite(Config) ->
    Config.

init_per_group(_, Config) ->
    ok = dispatcher:start(),
    Config.

end_per_group(_, _Config) ->
    ok = dispatcher:stop(),
    ok.

init_per_testcase(_, Config) ->
    Config.

end_per_testcase(_, _Config) ->
    ok.

%%%===================================================================
%%% Test Cases
%%%===================================================================
test_run(_Config) ->
  ct:log("it works!", []),
  ok.
