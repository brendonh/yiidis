%%%-------------------------------------------------------------------
%%% @author Brendon Hogger <brendonh@dev.brendonh.org>
%%% @copyright (C) 2011, Brendon Hogger
%%% @doc
%%% Dispatch tasks
%%% @end
%%% Created : 25 Aug 2011 by Brendon Hogger <brendonh@dev.brendonh.org>
%%%-------------------------------------------------------------------
-module(pt_handler).

-export([handle/1]).

-include("phptask.hrl").


handle(TaskKey) ->
    [Command, Rest] = binary:split(TaskKey, <<"::">>, []),
    Spec = binary:split(Command, <<":">>, [global]),
    get_handler(Spec, TaskKey).


get_handler([<<"yiic">>, Command | _Rest], TaskKey) ->
    handle_yiic(Command, TaskKey);

get_handler(UnknownCommand, TaskKey) ->
    ?INFO({unknown_command, UnknownCommand, TaskKey}),
    {error, <<"Unknown command">>, []}.


handle_yiic(Command, TaskKey) ->
    {ok, Yiic} = application:get_env(yiic),
    Port = open_port({spawn_executable, Yiic}, [{args, [Command, TaskKey]}, {line, 1024}, exit_status]),
    loop_port(Port, []).


loop_port(Port, Output) ->
    receive
        {Port, {data, {_EOL, Line}}} ->
            loop_port(Port, [Line|Output]);
        {Port, {exit_status, 0}} ->
            ok;
        {Port, {exit_status, Status}} ->
            BinStatus = list_to_binary(integer_to_list(Status)),
            BinOutput = lists:reverse(Output),
            {error, <<"Exit status: ", BinStatus/binary>>, [{<<"output">>, BinOutput}]}
    end.
