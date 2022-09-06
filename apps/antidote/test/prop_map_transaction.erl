-module(prop_map_transaction).
% -include_lib("proper/include/proper.hrl").

% %% Model Callbacks
% -export([command/1, initial_state/0, next_state/3,
%          precondition/2, postcondition/3]).

% -record(state, {map, node, object, clock}).

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
%     Bucket = test_bucket_map, 
%     Type = antidote_crdt_map_rr,
%     Key = antidote_prop_map_stful,
%     Node = persistent_term:get(node),
%     Object = {Key, Type, Bucket},
%     {ok, [Val], Clock} = rpc:call(Node, antidote, read_objects, [ignore, [], [Object]]),
%     io:format("map init : ~p ~n", [Val]),
%     State = #state{map = Val, node = Node, object = Object, clock = Clock},
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

% postcondition(_State, {call, _Mod, _Fun, [_, _, update_objects, [_, _, [{_, _X, _Y}]]]}, _Res) ->
%     true;
%     % io:format("result post cond : ~p ~n ~p ~n ~p ~n", [Res, X, Y]),
%     % io:format("=---------------"),
%     % {Ok, _} = Res,
%     % Ok =:= ok;
% postcondition(State, {call, _Mod, _Fun, [_, _, read_objects, _]}, Res) ->
%     {ok, [Map], _} = Res,
%     io:format("postcond check "),
%     io:format("stateset : ~p ~n", [State#state.map]),
%     io:format("set : ~p ~n", [Map]),
%     Map =:= State#state.map.
%     % equalSets(State#state.result, Set).

% %% @doc Assuming the postcondition for a call was true, update the model
% %% accordingly for the test to proceed.

% next_state(State, {ok, Clock}, {call, _Mod, _Fun, [_, _, update_objects, [_, _, [{_, update, {Term, Int}}]]]}) ->
%     io:format("update "),
%     NewState = State#state{map = {Term, Int}, clock = Clock},
%     NewState;
% next_state(State, {ok, Clock}, {call, _Mod, _Fun, [_, _, update_objects, [_, _, [{_, remove, Term}]]]}) ->
%     io:format("remove "),
%     NewState = State#state{map = Term, clock = Clock},
%     NewState;
% next_state(State, _Res, {call, _Mod, _Fun, [_, _, update_objects, _]}) ->
%     io:format("else "),
%     State;
% next_state(State, {ok, _, Clock}, {call, _Mod, _Fun, [_, _, read_objects, _]}) ->
%     io:format("read "),
%     NewState = State#state{clock = Clock},
%     NewState;
% next_state(State, _Res, {call, _Mod, _Fun, [_, _, read_objects, _]}) ->
%     io:format("read-else   "),
%     State.

% %%%%%%%%%%%%%%%%%%
% %%%%% HELPERS %%%%
% %%%%%%%%%%%%%%%%%%
% % setVal() -> oneof([
% %         a, b, c, d, e
% %     ]).

% % args() ->
% %     X = oneof([
% %         int(),
% %         int(),
% %         int()
% %         % {add, setVal()},
% %         % {remove, setVal()},
% %         % {add_all, list(setVal())},
% %         % {remove_all, list(setVal())}
% %         ]),
% %     io:format("args is this  ~p ~n ", [X]),
% %     {add, X}.


% op(State) ->
%     {State#state.object, update,  oneof([
%         {{update, reg_element(), crdt_op()}, crdt_op()},
%         {{remove, reg_element()}, crdt_op()}

%     ])}.

% crdt_op() ->
%     oneof([antidote_crdt_set_rw, antidote_crdt_map_rr]).

% reg_element() ->
%     oneof([a, b, c, d, e, f, g, h, i]).