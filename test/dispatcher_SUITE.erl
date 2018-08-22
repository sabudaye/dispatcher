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
        test_module_call_default_group/1,
        test_module_call/1,
        test_module_call_no_rules/1,
        test_message/1,
        test_group_message/1
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
                test_module_call_default_group,
                test_module_call,
                test_module_call_no_rules,
                test_message,
                test_group_message
            ]
        }
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
    {ok, Pid} = dispatcher_test_receiver:start_link(),
    dispatcher:register_receiver_process(dispatcher_test_receiver, Pid),
    Config.

end_per_testcase(_, _Config) ->
    ok.

%%%===================================================================
%%% Test Cases
%%%===================================================================
test_module_call_default_group(_Config) ->
    ok = dispatcher:command(test_command, test_args),
    ok.

test_module_call(_Config) ->
    ok = dispatcher:command({test_app2, test_command}, test_args),
    ok.

test_module_call_no_rules(_Config) ->
    ok = dispatcher:command({test_app2, test2}, test_args),
    ok.

test_message(_Config) ->
    ok = dispatcher:command({dispatcher_test_receiver, message_to_some_process}, test_args),
    ok.

test_group_message(_Config) ->
    ok = dispatcher:command({dispatcher_test_receiver, message_to_group}, test_args),
    ok.

