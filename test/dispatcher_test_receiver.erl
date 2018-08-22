%%%-------------------------------------------------------------------
%%
%%  Dispatcher command handler module
%%
%%%-------------------------------------------------------------------

-module(dispatcher_test_receiver).

-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

%% ===================================================================
%% API functions
%% ===================================================================

-spec start_link() -> {ok, pid()} | ignore | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% ===================================================================
%% gen_server callbacks
%% ===================================================================

init([]) ->
    {ok, #{}}.

handle_call(_Msg, _From, State) ->
    {reply, ok, State}.

handle_cast({Message, Args}, State) ->
    lager:info("got message ~p with args ~p", [Message, Args]),
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
