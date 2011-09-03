%%%-------------------------------------------------------------------
%%% @author Brendon Hogger <brendonh@dev.brendonh.org>
%%% @copyright (C) 2011, Brendon Hogger
%%% @doc
%%%
%%% @end
%%% Created : 16 Aug 2011 by Brendon Hogger <brendonh@dev.brendonh.org>
%%%-------------------------------------------------------------------
-module(pt_worker_sup).

-behaviour(supervisor).

%% API
-export([start_link/0, start_worker/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

start_worker() ->
    supervisor:start_child(?MODULE, []).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

init([]) ->

    Worker = {worker, {pt_worker, start_link, []},
              transient, 5000, worker, [pt_worker]},

    {ok, {{simple_one_for_one, 1, 5}, [Worker]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
