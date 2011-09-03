%%%-------------------------------------------------------------------
%%% @author Brendon Hogger <brendonh@dev.brendonh.org>
%%% @copyright (C) 2011, Brendon Hogger
%%% @doc
%%% Random helpful stuff
%%% @end
%%% Created : 29 Mar 2011 by Brendon Hogger <brendonh@dev.brendonh.org>
%%%-------------------------------------------------------------------
-module(pt_util).

-include("phptask.hrl").

-compile(export_all).

%%====================================================================
%% UUIDs
%%====================================================================

parse_uuid(S) ->
    I = erlang:list_to_integer([C || C <- S, C /= $-], 16),
    <<I:16/unsigned-integer-unit:8>>.

format_uuid(<<TL:32, TM:16, THV:16, CSR:8, CSL:8, N:48>>) -> 
    lists:flatten(io_lib:format("~8.16.0b-~4.16.0b-~4.16.0b-~2.16.0b~2.16.0b-~12.16.0b", 
                                [TL, TM, THV, CSR, CSL, N])).
    
random_uuid() -> 
    {A1,A2,A3} = erlang:now(),
    random:seed(A1, A2, A3),
    <<(random:uniform(4294967296) - 1):32,
      (random:uniform(4294967296) - 1):32,
      (random:uniform(4294967296) - 1):32,
      (random:uniform(4294967296) - 1):32>>.


%%====================================================================
%% Datetimes
%%====================================================================

datetime_now_diff(DT) ->
    datetime_diff(DT, calendar:universal_time()).

datetime_diff(DT2, DT1) ->            
    S2 = calendar:datetime_to_gregorian_seconds(DT2),
    S1 = calendar:datetime_to_gregorian_seconds(DT1),
    S2 - S1.


datetime_now_delta(Secs) ->
    datetime_delta(calendar:universal_time(), Secs).

datetime_delta(DT, Secs) ->
    S = calendar:datetime_to_gregorian_seconds(DT),
    calendar:gregorian_seconds_to_datetime(S + Secs).


unix_timestamp() ->
    unix_timestamp(calendar:universal_time()).

unix_timestamp(DT) ->
    Epoch = {{1970,1,1},{0,0,0}},
    calendar:datetime_to_gregorian_seconds(DT) -
        calendar:datetime_to_gregorian_seconds(Epoch).


