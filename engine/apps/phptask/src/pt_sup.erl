-module(pt_sup).

-behaviour(supervisor).

-include("phptask.hrl").

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).


%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->

    Watcher = {watcher, {pt_watcher, start_link, []},
               permanent, 5000, worker, [pt_watcher]},

    WorkerSup = {worker_sup, {pt_worker_sup, start_link, []},
                 permanent, 5000, supervisor, [pt_worker_sup]},

    {ok, { {one_for_one, 5, 10}, [Watcher, WorkerSup]} }.
