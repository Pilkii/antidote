-module(prop_transaction_interactive).
% -include_lib("proper/include/proper.hrl").

% %% Model Callbacks
% -export([command/1, initial_state/0, next_state/3, 
%          precondition/2, postcondition/3]).

% -record(state, {count, node, object, id}).

% %%%%%%%%%%%%%%%%%%
% %%% PROPERTIES %%%
% %%%%%%%%%%%%%%%%%%
% prop_test() ->
%     ?FORALL(Cmds, commands(?MODULE),
%             begin
%                 {History, State, Result} = run_commands(?MODULE, Cmds),
%                 ?WHENFAIL(io:format("History: ~p\nState: ~p\nResult: ~p\n",
%                                     [History,State,Result]),
%                           aggregate(command_names(Cmds), Result =:= ok))
%             end).

% prop_test_parallel() ->
%     ?FORALL(Cmds, parallel_commands(?MODULE),
%             begin
%                 {History, State, Result} = run_parallel_commands(?MODULE, Cmds),
%                 ?WHENFAIL(io:format("History: ~p\nState: ~p\nResult: ~p\n",
%                                     [History,State,Result]),
%                           aggregate(command_names(Cmds), Result =:= ok))
%             end).

% %%%%%%%%%%%%%
% %%% MODEL %%%
% %%%%%%%%%%%%%
% %% @doc Initial model value at system start. Should be deterministic.
% initial_state() ->
%     % Bucket = test_utils:bucket(antidote_bucket), with this bucket it does not work!!
%     Bucket = my_bucket,
%     Type = antidote_crdt_counter_pn,
%     Key = antidote_key_static_prop_stful,
%     Node = proplists:get_value(node, test_utils:init_prop_single_dc(?MODULE, #{})),
%     Object = {Key, Type, Bucket},
%     {ok, TxId} = rpc:call(Node, antidote, start_transaction, [ignore, []]),
%     {ok, [Val]} = rpc:call(Node, antidote, read_objects, [[Object], TxId]),
%     State = #state{count = Val, node = Node, object = Object, id = TxId},
%     State.



% %% @doc List of possible commands to run against the system
% command(State) ->
%     Update = {State#state.object, op(), int()},
%     Node = State#state.node,
%     oneof([
%         {call, rpc, call, [Node, antidote, update_objects, [[Update], State#state.id]]},
%         {call, rpc, call, [Node, antidote, read_objects, [[State#state.object], State#state.id]]}
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
%     {ok, [Int]} = Res,
%     State#state.count =:= Int.


% %% @doc Assuming the postcondition for a call was true, update the model
% %% accordingly for the test to proceed.
% next_state(State, _Res, {call, _Mod, _Fun, [_, _, update_objects, [[{_, decrement, Int}], _]]}) ->
%     NewState = State#state{count = State#state.count - Int},
%     NewState;
% next_state(State, _Res, {call, _Mod, _Fun, [_, _, update_objects, [[{_, increment, Int}], _]]}) ->
%     NewState = State#state{count = State#state.count + Int},
%     NewState;
% next_state(State, _Res, {call, _Mod, _Fun, [_, _, read_objects, _]}) ->
%     NewState = State,
%     NewState.

% %%%%%%%%%%%%%%%%%%
% %%%%% HELPERS %%%%
% %%%%%%%%%%%%%%%%%%

% op() ->
%     oneof([
%         increment,
%         decrement
%         ]).