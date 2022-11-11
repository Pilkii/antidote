-module(prop_rrmap_single_dc).
-include_lib("proper/include/proper.hrl").

%% Model Callbacks
-export([command/1, initial_state/0, next_state/3,
         precondition/2, postcondition/3]).

-record(state, {map, node, object, clock}).

-define(CRDT, antidote_crdt_map_rr).

%%%%%%%%%%%%%%%%%%
%%% PROPERTIES %%%
%%%%%%%%%%%%%%%%%%
prop_rrmap_single_dc_test() ->
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
    Bucket = test_bucket_rrmap_single_dc, 
    Key = antidote_prop_rrmap_single_dc,
    Node = persistent_term:get(node),
    Type = ?CRDT,
    io:format("Key [~p]", [Type]),
    Object = {Key, Type, Bucket},
    {ok, [Val], NewClock} = rpc:call(Node, antidote, read_objects, [ignore, [], [Object]]),
    Map = model_adjustments_crdts:turnIntoErlangType(Val, ?CRDT),
    State = #state{map = Map, node = Node, object = Object, clock = NewClock},
    State.

%% @doc List of possible commands to run against the system
command(State) ->
    Node = State#state.node,
    Update = map_utils:mapOp(State, ?CRDT),
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

postcondition(#state{map = Map, node = Node, object = Object}, {call, _Mod, _Fun, [_, _, update_objects, [_, _, [{_, Op, OpArg}]]]}, {ok, Clock}) ->
    {ok, [ReadMap], _} = rpc:call(Node, antidote, read_objects, [Clock, [], [Object]]),
    io:format("adjust map called: ~p ", [Map]),
    NewMap = model_adjustments_crdts:adjustMap(Map, {Op, OpArg}, ?CRDT),
    X = model_adjustments_crdts:isEqual(NewMap, ReadMap, ?CRDT),
    if 
        not X -> io:format("Post False NewMap: ~p, ReadMap: ~p ", [NewMap, ReadMap])
                , X;
        true -> X
    end;

postcondition(#state{map = Map}, {call, _Mod, _Fun, [_, _, read_objects, _]}, Res) ->
    {ok, [ReadMap], _} = Res,
    X = model_adjustments_crdts:isEqual(Map, ReadMap, ?CRDT),
    if 
        not X -> io:format("Post False NewMap: ~p, ReadMap: ~p ", [Map, ReadMap])
                , X;
        true -> X
    end.

%% @doc Assuming the postcondition for a call was true, update the model
%% accordingly for the test to proceed.
next_state(State = #state{map = Map}, {ok, Clock}, {call, _Mod, _Fun, [_, _, update_objects, [_, _, [{_, OpType, OpArg}]]]}) ->
    NewMap = model_adjustments_crdts:adjustMap(Map, {OpType, OpArg}, ?CRDT),
    NewState = State#state{map = NewMap, clock = Clock},
    NewState;
next_state(State, {ok, _, Clock}, {call, _Mod, _Fun, [_, _, read_objects, _]}) ->
    NewState = State#state{clock = Clock},
    NewState;
next_state(State, {var, _}, {call, _Mod, _Fun, _}) ->
    State.


