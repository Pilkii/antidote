-module(prop_set_transaction).
% -include_lib("proper/include/proper.hrl").

% %% Model Callbacks
% -export([command/1, initial_state/0, next_state/3, propp_test/0
%          precondition/2, postcondition/3]).

% -record(state, {set, node, object, clock}).

% %%%%%%%%%%%%%%%%%%
% %%% PROPERTIES %%%
% %%%%%%%%%%%%%%%%%%
% propp_test() ->
%     ?FORALL(Cmds, commands(?MODULE),
%             begin
%                 {History, State, Result} = run_commands(?MODULE, Cmds),
%                 ct:pal("result [~p]", [Result]),
%                 ?WHENFAIL(io:format("History: ~p\nState: ~p\nResult: ~p\n",
%                                     [History,State,Result]),
%                           aggregate(command_names(Cmds), Result =:= ok))
%             end).


% %%%%%%%%%%%%%
% %%% MODEL %%%
% %%%%%%%%%%%%%
% %% @doc Initial model value at system start. Should be deterministic.
% initial_state() ->
%     Bucket = test_bucket_set, % test_utils:bucket(antidote_bucket), doesnt work with this
%     Type = antidote_crdt_set_aw,
%     Key = antidote_prop_set_stful,
%     Node = proplists:get_value(node, test_utils:init_prop_single_dc(?MODULE, #{})),
%     Object = {Key, Type, Bucket},
%     {ok, [Val], Clock} = rpc:call(Node, antidote, read_objects, [ignore, [], [Object]]),
%     State = #state{set = Val, node = Node, object = Object, clock = Clock},
%     State.

% %% @doc List of possible commands to run against the system
% command(State) ->
%     Node = State#state.node,
%     oneof([
%         {call, rpc, call, [Node, antidote, update_objects, [State#state.clock, [], [getUpdate(State)]]]},
%         {call, rpc, call, [Node, antidote, read_objects, [State#state.clock, [], [State#state.object]]]}
%     ]).

% %% @doc Determines whether a command should be valid under the
% %% current state.

% precondition(_State, {call, _Mod, _Fun, _Args}) ->
%     true.


% %% @doc Given the state `State' *prior* to the call
% %% `{call, Mod, Fun, Args}', determine whether the result
% %% `Res' (coming from the actual system) makes sense.

% postcondition(_State, {call, _Mod, _Fun, [_, _, update_objects, _]}, Res) ->
%     {Ok, _} = Res,
%     Ok =:= ok;
% postcondition(State, {call, _Mod, _Fun, [_, _, read_objects, _]}, Res) ->
%     {ok, [Set], _} = Res,
%     equalSets(State#state.set, Set).

% %% @doc Assuming the postcondition for a call was true, update the model
% %% accordingly for the test to proceed.

% next_state(State, Res, {call, _Mod, _Fun, [_, _, update_objects, [_, _, [{_, add, Elem}]]]}) ->
%     {ok, CT1} = Res,
%     NewState = State#state{set = sets:add_element(Elem, State#state.set), clock = CT1},
%     NewState;
% next_state(State, Res, {call, _Mod, _Fun, [_, _, update_objects, [_, _, [{_, remove, Elem}]]]}) ->
%     {ok, CT1} = Res,
%     NewState = State#state{set = sets:del_element(Elem, State#state.set), clock = CT1},
%     NewState;
% next_state(State, Res, {call, _Mod, _Fun, [_, _, update_objects, [_, _, [{_, add_all, ElemList}]]]}) ->
%     {ok, CT1} = Res,
%     Set2 = sets:from_list(ElemList),
%     NewState = State#state{set = sets:intersection(State#state.set, Set2), clock = CT1},
%     NewState;
% next_state(State, Res, {call, _Mod, _Fun, [_, _, update_objects, [_, _, [{_, remove_all, ElemList}]]]}) ->
%     {ok, CT1} = Res,
%     Set2 = sets:from_list(ElemList),
%     NewState = State#state{set = sets:subtract(State#state.set, Set2), clock = CT1},
%     NewState;
% next_state(State, Res, {call, _Mod, _Fun, [_, _, read_objects, _]}) ->
%     {ok, _, CT1} = Res,
%     NewState = State#state{clock = CT1},
%     NewState.

% %%%%%%%%%%%%%%%%%%
% %%%%% HELPERS %%%%
% %%%%%%%%%%%%%%%%%%

% args() ->
%     oneof([
%         {add, term()}, {remove, term()}, {add_all, list(term())}, {remove_all, list(term())}
%         ]).

% getUpdate(State) ->
%     {Op, Op_Param} = args(),
%     Update = {State#state.object, Op, Op_Param},
%     Update.

% equalSets(Set1, Set2) ->
%     sets:is_subset(Set1, Set2) and sets:is_subset(Set2, Set1).
