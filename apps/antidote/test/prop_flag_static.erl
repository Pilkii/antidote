-module(prop_flag_static).
-include_lib("proper/include/proper.hrl").

%% Model Callbacks
-export([command/1, initial_state/0, next_state/3,
         precondition/2, postcondition/3]).

-record(state, {flag, node, object, clock}).

%%%%%%%%%%%%%%%%%%
%%% PROPERTIES %%%
%%%%%%%%%%%%%%%%%%
prop_flag_test() ->
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
    Bucket = test_bucket_flag, 
    Type = antidote_crdt_flag_ew,
    Key = antidote_prop_flag_stful,
    Node = persistent_term:get(node),
    Object = {Key, Type, Bucket},
    {ok, [Val], Clock} = rpc:call(Node, antidote, read_objects, [ignore, [], [Object]]),
    io:format("flag init : ~p ~n", [Val]),
    State = #state{flag = Val, node = Node, object = Object, clock = Clock},
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

postcondition(#state{flag = Flag, node = Node, clock = Clock, object = Object}, {call, _Mod, _Fun, [_, _, update_objects, [_, _, [{_, Op, OpArg}]]]}, {ok, _}) ->
    {ok, [Val], _} = rpc:call(Node, antidote, read_objects, [Clock, [], [Object]]),
    adjustFlag(Flag, {Op, OpArg}) =:= Val;
postcondition(#state{flag = Flag}, {call, _Mod, _Fun, [_, _, read_objects, _]}, {ok, [Val], _}) ->
    Val =:= Flag.

%% @doc Assuming the postcondition for a call was true, update the model
%% accordingly for the test to proceed.

next_state(State = #state{flag = Flag}, {ok, Clock}, {call, _Mod, _Fun, [_, _, update_objects, [_, _, [{_, OpType, OpArg}]]]}) ->
    io:format("update "),
    NewFlag = adjustFlag(Flag, {OpType, OpArg}),
    NewState = State#state{flag = NewFlag, clock = Clock},
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

adjustFlag(_Flag, {enable, {}}) ->
    true;
adjustFlag(_Flag, {disable, {}}) ->
    false;
adjustFlag(_Flag, {reset, {}}) ->
    false.

%%%%%%%%%%%%%%%%%%%%%
%%%%% GENERATORS %%%%
%%%%%%%%%%%%%%%%%%%%%


op(State) ->
    oneof([
        {State#state.object, enable, {}},
        {State#state.object, disable,{}},
        {State#state.object, reset, {}}
  ]).


