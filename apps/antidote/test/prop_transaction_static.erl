-module(prop_transaction_static).
% -include_lib("proper/include/proper.hrl").

% %% Model Callbacks
% -export([command/1, initial_state/0, next_state/3, 
%          precondition/2, postcondition/3]).

% -record(state, {count, node, object, clock}).

% %%%%%%%%%%%%%%%%%%
% %%% PROPERTIES %%%
% %%%%%%%%%%%%%%%%%%
% prop_test() ->
%     Node = proplists:get_value(node, test_utils:init_prop_single_dc(?MODULE, #{})),
%     persistent_term:put(node, Node),
%     ?FORALL(Cmds, commands(?MODULE),
%             begin
%                 {History, State, Result} = run_commands(?MODULE, Cmds),
%                  io:format("result : ~p ~n", [Result]),
%                 ?WHENFAIL(io:format("History: ~p\nState: ~p\nResult: ~p\n",
%                                     [History,State,Result]),
%                           aggregate(command_names(Cmds), Result =:= ok))
%             end).    


% %%%%%%%%%%%%%
% %%% MODEL %%%
% %%%%%%%%%%%%%
% %% @doc Initial model value at system start. Should be deterministic.
% initial_state() ->
%     Bucket = test_bucket, % test_utils:bucket(antidote_bucket), doesnt work with this
%     Type = antidote_crdt_counter_pn,
%     Key = antidote_key_static_prop_stful,
%     Node = persistent_term:get(node),
%     Object = {Key, Type, Bucket},
%     {ok, [Val], Clock} = rpc:call(Node, antidote, read_objects, [ignore, [], [Object]]),
%     State = #state{count = Val, node = Node, object = Object, clock = Clock},
%     State.

% %% @doc List of possible commands to run against the system
% command(State) ->
%     Update = {State#state.object, op(), int()},
%     Node = State#state.node,
%     oneof([
%         {call, rpc, call, [Node, antidote, update_objects, [State#state.clock, [], [Update]]]},
%         {call, rpc, call, [Node, antidote, read_objects, [State#state.clock, [], [State#state.object]]]}
%     ]).

% %% @doc Determines whether a command should be valid under the
% %% current state.
% precondition(_State, {call, _Mod, _Fun, _Args}) ->
%     true.


% %% @doc Given the state `State' *prior* to the call
% %% `{call, Mod, Fun, Args}', determine whether the result
% %% `Res' (coming from the actual system) makes sense.
% postcondition(_State, {call, _Mod, _Fun, [_, _, update_objects, _]}, _Res) ->
%     true;
% postcondition(State, {call, _Mod, _Fun, [_, _, read_objects, _]}, Res) ->
%     {ok, [Int], _} = Res,
%     State#state.count =:= Int.


% %% @doc Assuming the postcondition for a call was true, update the model
% %% accordingly for the test to proceed.

% next_state(State, {ok, Clock}, {call, _Mod, _Fun, [_, _, update_objects, [_, _, [{_, increment, Int}]]]}) ->
%     NewState = State#state{count = State#state.count + Int, clock = Clock},
%     NewState;
% next_state(State, {ok, Clock}, {call, _Mod, _Fun, [_, _, update_objects, [_, _, [{_, decrement, Int}]]]}) ->
%     NewState = State#state{count = State#state.count - Int, clock = Clock},
%     NewState;
% %% called when next state is not called by us 
% next_state(State, _Res, {call, _Mod, _Fun, [_, _, update_objects, _]}) ->
%     State;

% next_state(State, _Res, {call, _Mod, _Fun, [_, _, read_objects, _]}) ->
%     State.

% %%%%%%%%%%%%%%%%%%
% %%%%% HELPERS %%%%
% %%%%%%%%%%%%%%%%%%

% op() ->
%     oneof([
%         increment,
%         decrement
%         ]).