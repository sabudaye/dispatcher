%%%-------------------------------------------------------------------
%%
%% Command dispatcher test module
%%
%%%-------------------------------------------------------------------

-module(dispatcher_test_module).

%% API
-export([test_command/1]).

%%====================================================================
%% API functions
%%====================================================================

-spec test_command(any()) -> any().
test_command(Args) ->
    lager:info("test command called with args ~p", [Args]),
    ok.
