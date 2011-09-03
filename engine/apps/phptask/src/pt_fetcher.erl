%%%-------------------------------------------------------------------
%%% @author Brendon Hogger <brendonh@dev.brendonh.org>
%%% @copyright (C) 2011, Brendon Hogger
%%% @doc
%%%
%%% @end
%%% Created : 16 Aug 2011 by Brendon Hogger <brendonh@dev.brendonh.org>
%%%-------------------------------------------------------------------
-module(pt_fetcher).

-behaviour(gen_server).

-include("phptask.hrl").

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% Internal
-export([listen/0]).

-define(SERVER, ?MODULE). 

-define(LISTENERS, 1).

-record(state, {}).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).


%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    [spawn_link(?MODULE, listen, []) || _ <- lists:seq(1, ?LISTENERS)],
    {ok, #state{}}.


handle_call(Request, _From, State) ->
    ?INFO({unknown_call, Request}),
    {reply, ok, State}.


handle_cast({task, Queue, Task}, State) ->
    ?INFO({task, Queue, Task}),
    {noreply, State};

handle_cast(Msg, State) ->
    ?INFO({unknown_cast, Msg}),
    {noreply, State}.


handle_info(Info, State) ->
    ?INFO({unknown_info, Info}),
    {noreply, State}.


terminate(_Reason, _State) ->
    ok.


code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%%%===================================================================
%%% Internal functions
%%%===================================================================

listen() ->
    {ok, [Queue, Task]} = eredis:q(redis, ["BLPOP", "testq", "0"], infinity),
    gen_server:cast(?MODULE, {task, Queue, Task}),
    listen().
