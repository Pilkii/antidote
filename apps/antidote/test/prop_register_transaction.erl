-module(prop_register_transaction).
% -include_lib("proper/include/proper.hrl").

% %% Model Callbacks
% -export([command/1, initial_state/0, next_state/3,
%          precondition/2, postcondition/3]).

% -record(state, {reg, node, object, clock}).

% %%%%%%%%%%%%%%%%%%
% %%% PROPERTIES %%%
% %%%%%%%%%%%%%%%%%%
% prop_test() ->
%     Node = proplists:get_value(node, test_utils:init_prop_single_dc(?MODULE, #{})),
%     persistent_term:put(node, Node),
%     ?FORALL(Cmds, commands(?MODULE),
%             begin
%                 {History, State, Result} = run_commands(?MODULE, Cmds),
%                 io:format("result [~p]", [Result]),
%                 ?WHENFAIL(io:format("History: ~p\nState: ~p\nResult: ~p\n",
%                                     [History,State,Result]),
%                           aggregate(command_names(Cmds), Result =:= ok))
%             end).


% %%%%%%%%%%%%%
% %%% MODEL %%%
% %%%%%%%%%%%%%
% %% @doc Initial model value at system start. Should be deterministic.
% initial_state() ->
%     Bucket = test_bucket_reg, 
%     Type = antidote_crdt_register_lww,
%     Key = antidote_prop_reg_stful,
%     Node = persistent_term:get(node),
%     Object = {Key, Type, Bucket},
%     {ok, [Val], Clock} = rpc:call(Node, antidote, read_objects, [ignore, [], [Object]]),
%     io:format("reg init : ~p ~n", [Val]),
%     State = #state{reg = Val, node = Node, object = Object, clock = Clock},
%     State.

% %% @doc List of possible commands to run against the system
% command(State) ->
%     Node = State#state.node,
%     Update = op(State),
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

% postcondition(#state{clock = Clock, object = Object, node = Node}, {call, _Mod, _Fun, [_, _, update_objects, [_, _, [{_, assign, Res}]]]}, _Res) ->
%     {ok, [Val], _} = rpc:call(Node, antidote, read_objects, [Clock, [], [Object]]),
%     Val =:= Res; 
% postcondition(State, {call, _Mod, _Fun, [_, _, read_objects, _]}, Res) ->
%     {ok, [Reg], _} = Res,
%     io:format("postcond check "),
%     io:format("stateset : ~p ~n", [State#state.reg]),
%     io:format("set : ~p ~n", [Reg]),
%     io:format("check res : ~p ~n", [Reg =:= State#state.reg]),
%     Reg =:= State#state.reg;
% postcondition(_State, {call, _Mod, _Fun, _Args}, _Res) ->
%     false.

% %% @doc Assuming the postcondition for a call was true, update the model
% %% accordingly for the test to proceed.

% %% for clarity we will let these two assign next state functions seperated
% next_state(State, {ok, Clock}, {call, _Mod, _Fun, [_, _, update_objects, [_, _, [{_, assign, {Term, Int}}]]]}) ->
%     NewState = State#state{reg = {Term, Int}, clock = Clock},
%     NewState;
% next_state(State, {ok, Clock}, {call, _Mod, _Fun, [_, _, update_objects, [_, _, [{_, assign, Term}]]]}) ->
%     NewState = State#state{reg = Term, clock = Clock},
%     NewState;
% next_state(State, {ok, _, Clock}, {call, _Mod, _Fun, [_, _, read_objects, _]}) ->
%     NewState = State#state{clock = Clock}, %%maybe this makes the system more subsceptible to errors
%     NewState;
% next_state(State, _Res, {call, _Mod, _Fun, _Args}) ->
%     State.

% %%%%%%%%%%%%%%%%%%
% %%%%% HELPERS %%%%
% %%%%%%%%%%%%%%%%%%
% op(State) ->
%   oneof([
%     {State#state.object, assign, {reg_element(), non_neg_integer()}},
%     {State#state.object, assign, reg_element()}
%   ]).


% reg_element() ->
%     oneof([a, b, c, d, e, f, g, h, i]).