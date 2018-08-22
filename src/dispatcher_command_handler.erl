%%%-------------------------------------------------------------------
%%
%%  Cpmmand dispatcher command handler
%%
%%%-------------------------------------------------------------------

-module(dispatcher_command_handler).

-behaviour(gen_server).

%% API
-export([start_link/1]).

-export([command/1]).

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

-spec start_link(list()) -> {ok, pid()} | ignore | {error, term()}.
start_link([]) ->
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

handle_cast({command, _Command}, State) ->
    {noreply, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Msg, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
