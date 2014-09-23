-module(vectorclock).

-include("floppy.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-export([get_clock/1, update_clock/3, get_clock_by_key/1,
         is_greater_than/2,
         get_clock_of_dc/2,
         set_clock_of_dc/3,
         get_clock_node/1,
         from_list/1,
         eq/2,lt/2,gt/2,le/2,ge/2, strict_ge/2, strict_le/2]).

-export_type([vectorclock/0]).

-type vectorclock() :: dict().

get_clock_by_key(Key) ->
    Logid = log_utilities:get_logid_from_key(Key),
    Preflist = log_utilities:get_preflist_from_logid(Logid),
    Indexnode = hd(Preflist),
    lager:info("Preflist of Key ~p vectorclock ~p", [Key, Indexnode]),
    riak_core_vnode_master:sync_command(
      Indexnode, get_clock, vectorclock_vnode_master).

-spec get_clock(Partition :: non_neg_integer())
               -> {ok, vectorclock()} | {error, term()}.
get_clock(Partition) ->
    %%Logid = log_utilities:get_logid_from_partition(Partition),
    %%Preflist = log_utilities:get_apl_from_logid(Logid, vectorclock),
    Indexnode = {Partition, node()},
    case riak_core_vnode_master:sync_command(
           Indexnode, get_clock, vectorclock_vnode_master) of
        {ok, Clock} ->
            {ok, Clock};
        {error, Reason} ->
            lager:info("Update vector clock failed: ~p",[Reason]),
            {error, Reason}
    end.

get_clock_node(Node) ->
    Preflist = riak_core_apl:active_owners(vectorclock),
    Prefnode = [{Partition, Node1} ||
                   {{Partition, Node1},_Type} <- Preflist, Node1 =:= Node],
    %% Take a random vnode
    {A1,A2,A3} = now(),
    _Seed = random:seed(A1, A2, A3),
    Index = random:uniform(length(Prefnode)),
    VecNode = lists:nth(Index, Prefnode),
    riak_core_vnode_master:sync_command(
      VecNode, get_clock, vectorclock_vnode_master).

-spec update_clock(Partition :: non_neg_integer(),
                   Dc_id :: term(), Timestamp :: non_neg_integer())
                  -> {ok, vectorclock()} | {error, term()}.
update_clock(Partition, Dc_id, Timestamp) ->
    Indexnode = {Partition, node()},
    lager:info("Updating Preflist of vectorclock ~p", [Indexnode]),
    case riak_core_vnode_master:sync_command(Indexnode,
                                             {update_clock, Dc_id, Timestamp},
                                             vectorclock_vnode_master) of
        {ok, Clock} ->
            {ok, Clock};
        {error, Reason} ->
            lager:info("Update vector clock failed: ~p",[Reason]),
            {error, Reason}
    end.

%% @doc Return true if Clock1 > Clock2
-spec is_greater_than(Clock1 :: vectorclock(), Clock2 :: vectorclock())
                     -> boolean().
is_greater_than(Clock1, Clock2) ->
    dict:fold( fun(Dcid, Time2, Result) ->
                       case dict:find(Dcid, Clock1) of
                           {ok, Time1} ->
                               case Time1 > Time2 of
                                   true ->
                                       Result;
                                   false ->
                                       false
                               end;
                           error -> %%Localclock has not observered some dcid
                               false
                       end
               end,
               true, Clock2).

get_clock_of_dc(Dcid, VectorClock) ->
    dict:find(Dcid, VectorClock).

set_clock_of_dc(DcId, Time, VectorClock) ->
    dict:update(DcId,
                fun(_Value) ->
                        Time
                end,
                Time,
                VectorClock).

from_list(List) ->
    dict:from_list(List).

eq(V1, V2) ->
    dict:fold( fun(Dcid, Time2, Result) ->
                       case dict:find(Dcid, V1) of
                           {ok, Time1} ->
                               case Time1 =:= Time2 of
                                   true ->
                                       Result;
                                   false ->
                                       false
                               end;
                           error ->
                               false
                       end
               end,
               true, V2).

le(V1, V2) ->
    dict:fold( fun(Dcid, Time2, Result) ->
                       case dict:find(Dcid, V1) of
                           {ok, Time1} ->
                               case Time1 =< Time2 of
                                   true ->
                                       Result;
                                   false ->
                                       false
                               end;
                           error ->
                               Result
                       end
               end,
               true, V2).

ge(V1,V2) ->
    dict:fold( fun(Dcid, Time2, Result) ->
                       case dict:find(Dcid, V1) of
                           {ok, Time1} ->
                               case Time1 >= Time2 of
                                   true ->
                                       Result;
                                   false ->
                                       false
                               end;
                           error ->
                               false
                       end
               end,
               true, V2).

lt(V1,V2) ->
    dict:fold( fun(Dcid, Time2, Result) ->
                       case dict:find(Dcid, V1) of
                           {ok, Time1} ->
                               case Time1 < Time2 of
                                   true ->
                                       Result;
                                   false ->
                                       false
                               end;
                           error ->
                               Result
                       end
               end,
               true, V2).

gt(V1,V2) ->
    dict:fold( fun(Dcid, Time2, Result) ->
                       case dict:find(Dcid, V1) of
                           {ok, Time1} ->
                               case Time1 > Time2 of
                                   true ->
                                       Result;
                                   false ->
                                       false
                               end;
                           error ->
                               false
                       end
               end,
               true, V2).

strict_ge(V1,V2) ->
    ge(V1,V2) and (not eq(V1,V2)).

strict_le(V1,V2) ->
    le(V1,V2) and (not eq(V1,V2)).

-ifdef(TEST).

vectorclock_test() ->
    V1 = vectorclock:from_list([{1,5},{2,4},{3,5},{4,6}]),
    V2 = vectorclock:from_list([{1,4}, {2,3}, {3,4},{4,5}]),
    V3 = vectorclock:from_list([{1,5}, {2,4}, {3,4},{4,5}]),
    V4 = vectorclock:from_list([{1,6},{2,3},{3,1},{4,7}]),
    V5 = vectorclock:from_list([{1,6},{2,7}]),
    ?assertEqual(gt(V1,V2), true),
    ?assertEqual(lt(V2,V1), true),
    ?assertEqual(gt(V1,V3), false),
    ?assertEqual(strict_ge(V1,V3), true),
    ?assertEqual(strict_ge(V1,V1), false),
    ?assertEqual(ge(V1,V4), false),
    ?assertEqual(le(V1,V4), false),
    ?assertEqual(eq(V1,V4), false),
    ?assertEqual(ge(V1,V5), false).

-endif.
