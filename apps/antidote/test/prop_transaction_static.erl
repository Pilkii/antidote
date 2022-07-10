-module(prop_transaction_static).
-include_lib("proper/include/proper.hrl").

%% Model Callbacks
-export([command/1, initial_state/0, next_state/3,
         precondition/2, postcondition/3]).

-record(state, {count, node, object}).

%%%%%%%%%%%%%%%%%%
%%% PROPERTIES %%%
%%%%%%%%%%%%%%%%%%
prop_test() ->
    ?FORALL(Cmds, commands(?MODULE),
            begin
                {History, State, Result} = run_commands(?MODULE, Cmds),
                ct:pal("result [~p]", [Result]),
                ?WHENFAIL(io:format("History: ~p\nState: ~p\nResult: ~p\n",
                                    [History,State,Result]),
                          aggregate(command_names(Cmds), Result =:= ok))
            end).

prop_test_stless() ->
    Node = proplists:get_value(node, test_utils:init_prop_single_dc(what, #{})),
    Bucket = test_utils:bucket(antidote_bucket),
    Type = antidote_crdt_counter_pn,
    ?FORALL(Var, {op(), int()}, 
        begin
            Key = list_to_atom("antidote_key_static_prop" ++ float_to_list(rand:uniform())), %to avoid using the same key twice
            Object = {Key, Type, Bucket},
            {_, Int} = Var, 
            Update = {Object, increment, Int},
            {ok, _} = rpc:call(Node, antidote, update_objects, [ignore, [], [Update]]),
            {ok, [Val], _} = rpc:call(Node, antidote, read_objects, [ignore, [], [Object]]),
            Val =:= Int
        end
    ).

%%%%%%%%%%%%%
%%% MODEL %%%
%%%%%%%%%%%%%
%% @doc Initial model value at system start. Should be deterministic.
initial_state() ->
    Bucket = test_bucket, % test_utils:bucket(antidote_bucket), doesnt work with this
    Type = antidote_crdt_counter_pn,
    Key = antidote_key_static_prop_stful,
    Node = proplists:get_value(node, test_utils:init_prop_single_dc(?MODULE, #{})),
    Object = {Key, Type, Bucket},
    {ok, [Val], _} = rpc:call(Node, antidote, read_objects, [ignore, [], [Object]]),
    State = #state{count = Val, node = Node, object = Object},
    State.

%% @doc List of possible commands to run against the system
command(State) ->
    Update = {State#state.object, op(), int()},
    Node = State#state.node,
    oneof([
        {call, rpc, call, [Node, antidote, update_objects, [ignore, [], [Update]]]},
        {call, rpc, call, [Node, antidote, read_objects, [ignore, [], [State#state.object]]]}
    ]).

%% @doc Determines whether a command should be valid under the
%% current state.
precondition(_State, {call, _Mod, _Fun, _Args}) ->
    true.


%% @doc Given the state `State' *prior* to the call
%% `{call, Mod, Fun, Args}', determine whether the result
%% `Res' (coming from the actual system) makes sense.
postcondition(_State, {call, _Mod, _Fun, [_, _, update_objects, _]}, _Res) ->
    true;
postcondition(State, {call, _Mod, _Fun, [_, _, read_objects, _]}, Res) ->
    {ok, [Int], _} = Res,
    State#state.count =:= Int.


%% @doc Assuming the postcondition for a call was true, update the model
%% accordingly for the test to proceed.
next_state(State, _Res, {call, _Mod, _Fun, [_, _, update_objects, [_, _, [{_, increment, Int}]]]}) ->
    NewState = State#state{count = State#state.count + Int},
    NewState;
next_state(State, _Res, {call, _Mod, _Fun, [_, _, update_objects, [_, _, [{_, decrement, Int}]]]}) ->
    NewState = State#state{count = State#state.count - Int},
    NewState;
next_state(State, _Res, {call, _Mod, _Fun, [_, _, read_objects, _]}) ->
    NewState = State,
    NewState.

%%%%%%%%%%%%%%%%%%
%%%%% HELPERS %%%%
%%%%%%%%%%%%%%%%%%

op() ->
    oneof([
        increment,
        decrement
        ]).