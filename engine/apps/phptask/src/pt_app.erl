-module(pt_app).

-behaviour(application).

-include("phptask.hrl").

%% API
-export([start/0]).

%% Application callbacks
-export([start/2, stop/1]).


start() ->
    application:start(lager),
    application:start(phptask).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    {ok, Sup} = pt_sup:start_link(),
    [pt_worker_sup:start_worker() || _ <- lists:seq(1, 1)],
    {ok, Sup}.


stop(_State) ->
    ok.
