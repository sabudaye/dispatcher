%%%-------------------------------------------------------------------
%%
%% Command dispatcher config wrapper
%%
%%%-------------------------------------------------------------------

-module(dispatcher_config).

%% API
-export([rulesets/0]).

%%====================================================================
%% API functions
%%====================================================================

-spec rulesets() -> term().
rulesets() ->
    application:get_env(dispatcher, rulesets, []).
