-module(prop_set_aw_transaction).
-include_lib("proper/include/proper.hrl").

%% Model Callbacks
-export([command/1, initial_state/0, next_state/3, adjustSet/2,
         precondition/2, postcondition/3]).

-record(state, {set, node, object, clock}).

%%%%%%%%%%%%%%%%%%
%%% PROPERTIES %%%
%%%%%%%%%%%%%%%%%%
prop_set_test() ->
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
    Bucket = test_bucket_set, % test_utils:bucket(antidote_bucket), doesnt work with this
    Type = antidote_crdt_set_aw,
    Key = antidote_prop_set_stful,
    Node = persistent_term:get(node),
    Object = {Key, Type, Bucket},
    {ok, [Val], Clock} = rpc:call(Node, antidote, read_objects, [ignore, [], [Object]]),
    State = #state{set = Val, node = Node, object = Object, clock = Clock},
    State.

%% @doc List of possible commands to run against the system
command(State) ->
    Node = State#state.node,
    Update = op(State),
    oneof([
        {call, rpc, call, [Node, antidote, update_objects, [State#state.clock, [], [Update]]]},
        {call, rpc, call, [Node, antidote, read_objects, [State#state.clock, [], [State#state.object]]]}
    ]).

%% @doc Determines whether a command should be valid under the
%% current state.

precondition(_State, {call, _Mod, _Fun, _Args}) ->
    true.


%% @doc Given the state `State' *prior* to the call
%% `{call, Mod, Fun, Args}', determine whether the result
%% `Res' (coming from the actual system) makes sense.

postcondition(#state{set = PreSet, clock = Clock, node = Node, object = Object}, {call, _Mod, _Fun, [_, _, update_objects, [_, _, [{_, add, Elem}]]]}, {ok, _}) ->
    {ok, [Set], _} = rpc:call(Node, antidote, read_objects, [Clock, [], [Object]]),
    ResSet = lists:usort(lists:append([Elem], PreSet)),
    equalSets(Set, ResSet);
postcondition(#state{set = PreSet, clock = Clock, node = Node, object = Object}, {call, _Mod, _Fun, [_, _, update_objects, [_, _, [{_, remove, Elem}]]]}, {ok, _}) ->
    {ok, [Set], _} = rpc:call(Node, antidote, read_objects, [Clock, [], [Object]]),
    ResSet = lists:delete(Elem, PreSet),
    equalSets(Set, ResSet);
postcondition(#state{set = PreSet, clock = Clock, node = Node, object = Object}, {call, _Mod, _Fun, [_, _, update_objects, [_, _, [{_, add_all, ElemList}]]]}, {ok, _}) ->
    {ok, [Set], _} = rpc:call(Node, antidote, read_objects, [Clock, [], [Object]]),
    ResSet = lists:usort(lists:append(PreSet, ElemList)),
    equalSets(Set, ResSet);
postcondition(#state{set = PreSet, clock = Clock, node = Node, object = Object}, {call, _Mod, _Fun, [_, _, update_objects, [_, _, [{_, remove_all, ElemList}]]]}, {ok, _}) ->
    {ok, [Set], _} = rpc:call(Node, antidote, read_objects, [Clock, [], [Object]]),
    ResSet = lists:subtract(PreSet, ElemList),
    equalSets(Set, ResSet);
postcondition(#state{set = _PreSet, clock = Clock, node = Node, object = Object}, {call, _Mod, _Fun, [_, _, update_objects, [_, _, [{_, reset, {}}]]]}, {ok, _}) ->
    {ok, [Set], _} = rpc:call(Node, antidote, read_objects, [Clock, [], [Object]]),
    equalSets(Set, []);
postcondition(#state{set = Set}, {call, _Mod, _Fun, [_, _, read_objects, _]}, {ok, [ResSet], _}) ->
    equalSets(Set, ResSet);
postcondition(_State, {call, _Mod, _Fun, _Args}, _Res) ->
    false.

%% @doc Assuming the postcondition for a call was true, update the model
%% accordingly for the test to proceed.

next_state(State = #state{set = Set}, {ok, Clock}, {call, _Mod, _Fun, [_, _, update_objects, [_, _, [{_, Op, Elem}]]]}) ->
    ResSet = adjustSet(Set, {Op, Elem}),
    NewState = State#state{set = ResSet, clock = Clock},
    NewState;
next_state(State, _Res, {call, _Mod, _Fun, [_, _, update_objects, _]}) ->
    State;
next_state(State, {ok, _, Clock}, {call, _Mod, _Fun, [_, _, read_objects, _]}) ->
    NewState = State#state{clock = Clock},
    NewState;
next_state(State, _Res, {call, _Mod, _Fun, [_, _, read_objects, _]}) ->
    State.

%%%%%%%%%%%%%%%%%%
%%%%% HELPERS %%%%
%%%%%%%%%%%%%%%%%%

adjustSet(_, {reset, {}}) ->
    [];

adjustSet(Set, {add, Elem}) ->
    lists:usort(lists:append([Elem], Set));

adjustSet(Set, {remove, Elem}) ->
    lists:delete(Elem, Set);

adjustSet(Set, {add_all, ElemList}) ->
    lists:usort(lists:append(ElemList, Set));

adjustSet(Set, {remove_all, ElemList}) ->
    lists:subtract(Set, ElemList).


%%%%%%%%%%%%%%%%%%%%%
%%%%% GENERATORS %%%%
%%%%%%%%%%%%%%%%%%%%%


equalSets(Set1, Set2) ->
    X = lists:subtract(Set1, Set2) =:= lists:subtract(Set2, Set1),
    X.

op(State) ->
  oneof([
    {State#state.object, add, set_element()},
    {State#state.object, add_all, list(set_element())},
    {State#state.object, remove, set_element()},
    {State#state.object, remove_all, list(set_element())},
    {State#state.object, reset, {}}
  ]).

set_element() ->
    oneof([
        integer(0, 20),
        atom()]).