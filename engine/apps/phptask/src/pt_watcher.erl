%%%-------------------------------------------------------------------
%%% @author Brendon Hogger <brendonh@dev.brendonh.org>
%%% @copyright (C) 2011, Brendon Hogger
%%% @doc
%%% Make sure that *something* happens to every task.
%%% @end
%%% Created : 17 Aug 2011 by Brendon Hogger <brendonh@dev.brendonh.org>
%%%-------------------------------------------------------------------
-module(pt_watcher).

-behaviour(gen_server).

-include("phptask.hrl").

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).


-define(SERVER, ?MODULE). 

-define(POLL_TIME, 5000).

-define(KEY(Suffix), <<(State#state.prefix)/binary, ":", Suffix/binary >>).

% XXX TODO: Make this per-task
-define(TIMEOUT, 10).

-record(state, {
  prefix,
  callClient
}).


%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).


%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    {ok, Prefix} = application:get_env(redis_prefix),
    {ok, CallClient} = erldis_client:start_link(),
    State = #state{
      prefix=Prefix,
      callClient=CallClient
     },
    timer:send_interval(?POLL_TIME, check_pending),
    ?INFO({watcher_running, Prefix}),
    {ok, State}.


handle_call(Request, _From, State) ->
    ?INFO({unexpected_call, Request}),
    {reply, ok, State}.


handle_cast(Msg, State) ->
    ?INFO({unexpected_cast, Msg}),
    {noreply, State}.


handle_info(check_pending, State) ->
    Threshold = pt_util:unix_timestamp() - ?TIMEOUT,
    C = State#state.callClient,
    TimedOut = erldis_client:scall(C, [<<"ZRANGEBYSCORE">>, ?KEY(<<"timeouts">>), <<"-inf">>, Threshold]),
    [timeout_task(T, State) || T <- TimedOut],
    {noreply, State};

handle_info(Info, State) ->
    ?INFO({unexpected_info, Info}),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%%%===================================================================
%%% Internal functions
%%%===================================================================


timeout_task(Task, State) ->
    case erldis_client:scall(
           State#state.callClient, 
           [<<"ZRANK">>, ?KEY(<<"timeouts">>), Task]) of
        [nil] ->
            ok;
        _Existing ->
            ?INFO({timed_out, Task}),
            pt_worker:fail_task(Task, <<"timeout">>, [], State)
    end.

