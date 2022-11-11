-module(prop_pncounter_single_dc).
-include_lib("proper/include/proper.hrl").

%% Model Callbacks
-export([command/1, initial_state/0, next_state/3,
         precondition/2, postcondition/3]).

-record(state, {model, node, object, clock}).

-define(CRDT, antidote_crdt_counter_pn).

%%%%%%%%%%%%%%%%%%
%%% PROPERTIES %%%
%%%%%%%%%%%%%%%%%%
prop_pncounter_single_dc_test() ->
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
    Bucket = test_bucket_pncounter_single_dc, 
    Key = antidote_prop_pncounter_single_dc,
    Node = persistent_term:get(node),
    Type = ?CRDT,
    Object = {Key, Type, Bucket},
    {ok, [Val], NewClock} = rpc:call(Node, antidote, read_objects, [ignore, [], [Object]]),
    Model = model_adjustments_crdts:turnIntoErlangType(Val, ?CRDT),
    State = #state{model = Model, node = Node, object = Object, clock = NewClock},
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

postcondition(#state{model = Model, node = Node, object = Object}, {call, _Mod, _Fun, [_, _, update_objects, [_, _, [{_, Op, OpArg}]]]}, {ok, Clock}) ->
    {ok, [ReadModel], _} = rpc:call(Node, antidote, read_objects, [Clock, [], [Object]]),
    NewModel = model_adjustments_crdts:adjustModel(Model, {Op, OpArg}, ?CRDT),
    X = model_adjustments_crdts:isEqual(NewModel, ReadModel, ?CRDT),
    if 
        not X -> io:format("Post False NewMap: ~p, ReadMap: ~p ", [NewModel, ReadModel])
                , X;
        true -> X
    end;

postcondition(#state{model = Model}, {call, _Mod, _Fun, [_, _, read_objects, _]}, Res) ->
    {ok, [ReadModel], _} = Res,
    X = model_adjustments_crdts:isEqual(Model, ReadModel, ?CRDT),
    if 
        not X -> io:format("Post False NewMap: ~p, ReadMap: ~p ", [Model, ReadModel])
                , X;
        true -> X
    end.

%% @doc Assuming the postcondition for a call was true, update the model
%% accordingly for the test to proceed.
next_state(State = #state{model = Model}, {ok, Clock}, {call, _Mod, _Fun, [_, _, update_objects, [_, _, [{_, OpType, OpArg}]]]}) ->
    NewModel = model_adjustments_crdts:adjustModel(Model, {OpType, OpArg}, ?CRDT),
    NewState = State#state{model = NewModel, clock = Clock},
    NewState;
next_state(State, {ok, _, Clock}, {call, _Mod, _Fun, [_, _, read_objects, _]}) ->
    NewState = State#state{clock = Clock},
    NewState;
next_state(State, {var, _}, {call, _Mod, _Fun, _}) ->
    State.


%%%%%%%%%%%%%%%%%%%%%
%%%%% GENERATORS %%%%
%%%%%%%%%%%%%%%%%%%%%

op(State) ->
    oneof([
        {State#state.object, increment, ?SIZED(Size, resize(Size, int()))},
        {State#state.object, decrement, ?SIZED(Size, resize(Size, int()))}
  ]).
