%%%-------------------------------------------------------------------
%%% @author Brendon Hogger <brendonh@dev.brendonh.org>
%%% @copyright (C) 2011, Brendon Hogger
%%% @doc
%%%
%%% @end
%%% Created : 16 Aug 2011 by Brendon Hogger <brendonh@dev.brendonh.org>
%%%-------------------------------------------------------------------
-module(pt_worker).

-behaviour(gen_server).

-include("phptask.hrl").

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% Internal
-export([fetcher/2]).

-define(SERVER, ?MODULE). 

-define(KEY(Suffix), <<(State#state.prefix)/binary, ":", Suffix/binary >>).

-record(state, {
  prefix,
  client
}).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    gen_server:start_link(?MODULE, [], []).


%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    {ok, Prefix} = application:get_env(redis_prefix),
    {ok, Client} = erldis_client:start_link(),
    ?INFO({listener_running, Prefix}),
    State = #state{prefix=Prefix, client=Client},
    {ok, listen(State)}.


handle_call(Request, _From, State) ->
    ?INFO({unknown_call, Request}),
    {reply, ok, State}.


handle_cast({task, Queue, Task}, State) ->
    ?DBG({task, self(), Queue, Task}),
    case pt_handler:handle(Task) of
        ok -> 
            remove_task(Task, State);
        {error, Reason, Extra} ->
            fail_task(Task, Reason, Extra, State)
    end,
    {noreply, listen(State)};

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

listen(State) ->
    spawn_link(?MODULE, fetcher, [self(), State]),
    State.

fetcher(Worker, State) ->
    Client = State#state.client,
    Queue = ?KEY(<<"queue">>),
    [Task] = erldis_client:scall(Client, [<<"BRPOPLPUSH">>, Queue, ?KEY(<<"starting">>), <<"0">>], infinity),

    Now = pt_util:unix_timestamp(),
    erldis_client:scall(Client, [<<"ZADD">>, ?KEY(<<"timeouts">>), Now, Task]),
    erldis_client:scall(Client, [<<"LREM">>, ?KEY(<<"starting">>), <<"0">>, Task]),

    gen_server:cast(Worker, {task, Queue, Task}).



fail_task(Task, Reason, Extra, State) ->
    Now = pt_util:unix_timestamp(),
    Blob = {struct, 
            [{<<"task">>, Task},
             {<<"time">>, Now},
             {<<"reason">>, Reason}
             | Extra]},
    JSON = list_to_binary(mochijson2:encode(Blob)),
    
    erldis_client:scall(State#state.client, [<<"RPUSH">>, ?KEY(<<"failed">>), JSON]),
    remove_task(Task, State).
    
    

remove_task(Task, State) ->
    erldis_client:scall(State#state.client, [<<"ZREM">>, ?KEY(<<"timeouts">>), Task]).
