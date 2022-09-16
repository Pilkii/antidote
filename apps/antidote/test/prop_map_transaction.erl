-module(prop_map_transaction).
-include_lib("proper/include/proper.hrl").

%% Model Callbacks
-export([command/1, initial_state/0, next_state/3,
         precondition/2, postcondition/3]).

-record(state, {map, node, object, clock}).

%%%%%%%%%%%%%%%%%%
%%% PROPERTIES %%%
%%%%%%%%%%%%%%%%%%
prop_map_test() ->
    Node = proplists:get_value(node, test_utils:init_prop_single_dc(?MODULE, #{})),
    persistent_term:put(node, Node),
    ?FORALL(Cmds, commands(?MODULE),
            begin
                {History, State, Result} = run_commands(?MODULE, Cmds),
                io:format("result [~p]", [Result]),
                ?WHENFAIL(io:format("History: ~p\nState: ~p\nResult: ~p\n",
                                    [History,State,Result]),
                          aggregate(command_names(Cmds), Result =:= ok))
            end).


%%%%%%%%%%%%%
%%% MODEL %%%
%%%%%%%%%%%%%
%% @doc Initial model value at system start. Should be deterministic.
initial_state() ->
    Bucket = test_bucket_map, 
    Type = antidote_crdt_map_rr,
    Key = antidote_prop_map_stful,
    Node = persistent_term:get(node),
    Object = {Key, Type, Bucket},
    {ok, [Val], Clock} = rpc:call(Node, antidote, read_objects, [ignore, [], [Object]]),
    io:format("map init : ~p ~n", [Val]),
    State = #state{map = maps:from_list(Val), node = Node, object = Object, clock = Clock},
    State.

%% @doc List of possible commands to run against the system
command(State) ->
    Node = State#state.node,
    Update = op(State),
    frequency([
        {4, {call, rpc, call, [Node, antidote, update_objects, [State#state.clock, [], [Update]]]}},
        {1, {call, rpc, call, [Node, antidote, read_objects, [State#state.clock, [], [State#state.object]]]}}
    ]).

%% @doc Determines whether a command should be valid under the
%% current state.

precondition(_State, {call, _Mod, _Fun, _Args}) ->
    true.


%% @doc Given the state `State' *prior* to the call
%% `{call, Mod, Fun, Args}', determine whether the result
%% `Res' (coming from the actual system) makes sense.

postcondition(#state{map = Map, node = Node, clock = Clock, object = Object}, {call, _Mod, _Fun, [_, _, update_objects, [_, _, [{_, Op, OpArg}]]]}, {ok, _}) ->
    {ok, [Val], _} = rpc:call(Node, antidote, read_objects, [Clock, [], [Object]]),
    NewMap = adjustMap(Map, {Op, OpArg}),
    MapInList = maps:to_list(NewMap),
    io:format("postcond check with  ~p ~n ~p ~n", [Op, OpArg]),
    io:format("map pre operation: ~p ~n", [Map]),
    io:format("ourmap: ~p ~n antidotemap: ~p ~n", [MapInList, Val]),
    lists:subtract(MapInList, Val) =:= lists:subtract(Val, MapInList);
postcondition(State, {call, _Mod, _Fun, [_, _, read_objects, _]}, Res) ->
    {ok, [Val], _} = Res,
    io:format("postcond check "),
    io:format("stateset : ~p ~n", [State#state.map]),
    MapInList = maps:to_list(State#state.map),
    lists:subtract(MapInList, Val) =:= lists:subtract(Val, MapInList).
    % equalSets(State#state.result, Set).

%% @doc Assuming the postcondition for a call was true, update the model
%% accordingly for the test to proceed.

next_state(State = #state{map = Map}, {ok, Clock}, {call, _Mod, _Fun, [_, _, update_objects, [_, _, [C]]]}) ->
    io:format("ELSE [~p] ~n", [C]),
    {_, OpType, OpArg} = C,
    io:format("update "),
    NewMap = adjustMap(Map, {OpType, OpArg}),
    NewState = State#state{map = NewMap, clock = Clock},
    NewState;
next_state(State, {ok, _, Clock}, {call, _Mod, _Fun, [_, _, read_objects, _]}) ->
    io:format("read "),
    NewState = State#state{clock = Clock},
    NewState;
next_state(State, _Res, {call, _Mod, _Fun, [_, _, update_objects, _]}) ->
    State;
next_state(State, _Res, {call, _Mod, _Fun, [_, _, read_objects, _]}) ->
    io:format("read-else   "),
    State.

%%%%%%%%%%%%%%%%%%
%%%%% HELPERS %%%%
%%%%%%%%%%%%%%%%%%

adjustMap(Map, {_, []}) ->
    Map;

adjustMap(Map, {update, [H]}) ->
    NewMap = adjustMap(Map, {update, H}),
    NewMap;

adjustMap(Map, {update, [H | T]}) ->
    NewMap = adjustMap(Map, {update, H}),
    adjustMap(NewMap, {update, T});

adjustMap(Map, {remove, [H]}) ->
    NewMap = adjustMap(Map, {remove, H}),
    NewMap;

adjustMap(Map, {remove, [H | T]}) ->
    NewMap = adjustMap(Map, {remove, H}),
    adjustMap(NewMap, {remove, T});

adjustMap(Map, {update, {{_Key, _Type}, []}}) ->
    Map;

adjustMap(Map, {update, {{Key, antidote_crdt_set_aw}, Op}}) ->
    FullKey = {Key, antidote_crdt_set_aw},
    IsKey = maps:is_key(FullKey, Map),
    if 
        IsKey ->
             Value = maps:get(FullKey, Map),
             NewSet = prop_set_aw_transaction:adjustSet(Value, Op);
        true -> 
             NewSet = prop_set_aw_transaction:adjustSet([], Op)
    end,
    if 
        NewSet =:= [] -> NewMap = maps:remove(FullKey, Map);
        true -> NewMap = maps:put(FullKey, NewSet, Map)
    end,
    NewMap;

adjustMap(Map, {update, {{Key, antidote_crdt_map_rr}, Op}}) ->
    FullKey = {Key, antidote_crdt_map_rr},
    IsKey = maps:is_key(FullKey, Map),
    if 
        IsKey -> 
            InnerMap = maps:get(FullKey, Map),
            NewInnerMap = adjustMap(maps:from_list(InnerMap), Op);
        true ->
            NewInnerMap = adjustMap(maps:from_list([]), Op)
    end,
    if 
        NewInnerMap =:= #{} -> NewMap = maps:remove(FullKey, Map);
        true -> NewMap = maps:put(FullKey, maps:to_list(NewInnerMap), Map)
    end,
    NewMap;

adjustMap(Map, {remove, {Key, Crdt}}) ->
    io:format("remove this key ~p from this map ~p ~n", [{Key, Crdt}, Map]),
    NewMap = maps:remove({Key, Crdt}, Map),
    io:format("results in: ~p ~n", [NewMap]),
    NewMap;

adjustMap(Map, {batch, {Update, Removes}}) ->
    NewMap = adjustMap(Map, {update, Update}),
    adjustMap(NewMap, {remove, Removes});

adjustMap(_Map, {reset, {}}) ->
    maps:new().

%%%%%%%%%%%%%%%%%%%%%
%%%%% GENERATORS %%%%
%%%%%%%%%%%%%%%%%%%%%


op(State) -> 
    ?SIZED(Size, op(State, Size)).
op(State, Size) ->
    oneof([
        {State#state.object, update, nestedOp(Size)},
        {State#state.object, update, ?LET(L, list(nestedOp(Size div 2)), removeDuplicateKeys(L, []))},
        {State#state.object, remove, typed_key()},
        {State#state.object, remove, ?LET(L, list(typed_key()), lists:usort(L))},
        ?LET({Updates, Removes},
        {list(nestedOp(Size div 2)), list(typed_key())},
        begin
            Removes2 = lists:usort(Removes),
            Updates2 = removeDuplicateKeys(Updates, Removes2),
            {State#state.object, batch, {Updates2, Removes2}}
        end),
        {State#state.object, reset, {}}
  ]).

recOp(Size) -> 
    oneof([
        {update, nestedOp(Size)},
        {update, ?LET(L, list(nestedOp(Size div 2)), removeDuplicateKeys(L, []))},
        {remove, typed_key()},
        {remove, ?LET(L, list(typed_key()), lists:usort(L))},
        ?LET({Updates, Removes},
        {list(nestedOp(Size div 2)), list(typed_key())},
        begin
            Removes2 = lists:usort(Removes),
            Updates2 = removeDuplicateKeys(Updates, Removes2),
            {batch, {Updates2, Removes2}}
        end),
        {reset, {}}
        ]).

removeDuplicateKeys([], _) -> [];
removeDuplicateKeys([{Key, Op}|Rest], Keys) ->
  case lists:member(Key, Keys) of
    true -> removeDuplicateKeys(Rest, Keys);
    false -> [{Key, Op}|removeDuplicateKeys(Rest, [Key|Keys])]
  end.

nestedOp(Size) ->
  oneof(
    [
      % {{key(), antidote_crdt_counter_fat}, prop_counter_fat:op()},
      % {{key(), antidote_crdt_set_aw}, prop_set_aw:op()},
      {{key(), antidote_crdt_set_aw}, setOp()}
    ]
    ++
    if
      Size > 1 ->
        [{{key(), antidote_crdt_map_rr}, ?LAZY(recOp(Size div 2))}];
      true -> []
    end
    ).

typed_key() -> {key(), crdt_type()}.

crdt_type() ->
  oneof([antidote_crdt_set_aw, antidote_crdt_map_rr]).

key() ->
  oneof([key1, key2, key3, key4]).

setOp() ->
  oneof([
    {add, set_element()},
    {add_all, list(set_element())},
    {remove, set_element()},
    {remove_all, list(set_element())},
    {reset, {}}
  ]).

set_element() ->
  oneof([a, b]).
