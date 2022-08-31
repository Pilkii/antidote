-module(prop_set_transaction).
-include_lib("proper/include/proper.hrl").

%% Model Callbacks
-export([command/1, initial_state/0, next_state/3,
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
    UD = getUpdate(State),
    % E = int(),
    % io:format("command call [~p]", [E()]),
    oneof([
        {call, rpc, call, [Node, antidote, update_objects, [State#state.clock, [], [UD]]]},
        {call, rpc, call, [Node, antidote, read_objects, [State#state.clock, [], [State#state.object]]]}
    ]).

%% @doc Determines whether a command should be valid under the
%% current state.

precondition(_State, {call, _Mod, _Fun, _Args}) ->
    true.


%% @doc Given the state `State' *prior* to the call
%% `{call, Mod, Fun, Args}', determine whether the result
%% `Res' (coming from the actual system) makes sense.

postcondition(_State, {call, _Mod, _Fun, [_, _, update_objects, [_, _, [{_, _X, _Y}]]]}, _Res) ->
    true;
    % io:format("result post cond : ~p ~n ~p ~n ~p ~n", [Res, X, Y]),
    % io:format("=---------------"),
    % {Ok, _} = Res,
    % Ok =:= ok;
postcondition(State, {call, _Mod, _Fun, [_, _, read_objects, _]}, Res) ->
    {ok, [Set], _} = Res,
    io:format("postcond check "),
    io:format("stateset : ~p ~n", [State#state.set]),
    io:format("set : ~p ~n", [Set]),
    equalSets(State#state.set, Set).

%% @doc Assuming the postcondition for a call was true, update the model
%% accordingly for the test to proceed.

next_state(State, {ok, Clock}, {call, _Mod, _Fun, [_, _, update_objects, [_, _, [{_, add, Elem}]]]}) ->
    io:format("add Set to this set: "),
    io:format("arg : ~p ~n", [Elem]),
    io:format("set : ~p ~n", [State#state.set]),
    io:format("result to this set: "),
    ResSet = lists:uniq(lists:append([Elem], State#state.set)),
    io:format("result set : ~p ~n", [ResSet]),
    NewState = State#state{set = ResSet, clock = Clock},
    NewState;
next_state(State, {ok, Clock}, {call, _Mod, _Fun, [_, _, update_objects, [_, _, [{_, remove, Elem}]]]}) ->
    io:format("remove Set to this set: "),
    io:format("arg : ~p ~n", [Elem]),
    io:format("set : ~p ~n", [State#state.set]),
    io:format("result to this set: "),
    ResSet = lists:delete(Elem, State#state.set),
    io:format("result set : ~p ~n", [ResSet]),
    NewState = State#state{set = ResSet, clock = Clock},
    NewState;
next_state(State, {ok, Clock}, {call, _Mod, _Fun, [_, _, update_objects, [_, _, [{_, add_all, ElemList}]]]}) ->
    io:format("add_all Set to this set: "),
    io:format("arg : ~p ~n", [ElemList]),
    io:format("set : ~p ~n", [State#state.set]),
    io:format("result to this set: "),
    ResSet = lists:uniq(lists:append(ElemList, State#state.set)),
    io:format("result set : ~p ~n", [ResSet]),
    NewState = State#state{set = ResSet, clock = Clock},
    NewState;
next_state(State, {ok, Clock}, {call, _Mod, _Fun, [_, _, update_objects, [_, _, [{_, remove_all, ElemList}]]]}) ->
    io:format("add_all Set to this set: "),
    io:format("arg : ~p ~n", [ElemList]),
    io:format("set : ~p ~n", [State#state.set]),
    io:format("result to this set: "),
    ResSet = lists:subtract(State#state.set, ElemList),
    io:format("result set : ~p ~n", [ResSet]),
    NewState = State#state{set = ResSet, clock = Clock},
    NewState;
next_state(State, Res, {call, _Mod, _Fun, [_, _, update_objects, _]}) ->
    io:format("next state cllaed without right res: "),
    io:format("arg : ~p ~n", [Res]),
    State;
next_state(State, {ok, _, Clock}, {call, _Mod, _Fun, [_, _, read_objects, _]}) ->
    NewState = State#state{clock = Clock},
    NewState;
next_state(State, _Res, {call, _Mod, _Fun, [_, _, read_objects, _]}) ->
    State.

%%%%%%%%%%%%%%%%%%
%%%%% HELPERS %%%%
%%%%%%%%%%%%%%%%%%
% setVal() -> oneof([
%         a, b, c, d, e
%     ]).

% args() ->
%     X = oneof([
%         int(),
%         int(),
%         int()
%         % {add, setVal()},
%         % {remove, setVal()},
%         % {add_all, list(setVal())},
%         % {remove_all, list(setVal())}
%         ]),
%     io:format("args is this  ~p ~n ", [X]),
%     {add, X}.

getUpdate(State) ->
    io:format("get Update called"),    
    X = op(),
    {Op, Op_Param} = X,
    io:format("args is this ~p ~n ", [X]),
    Update = {State#state.object, Op, Op_Param},
    io:format("update is this ~p ~n ", [Update]),
    Update.



equalSets(Set1, Set2) ->
    X = lists:subtract(Set1, Set2) =:= lists:subtract(Set2, Set1),
    io:format("set 1 and set 2 are the same? : ~p ~n", [X]),
    X.

op() ->
    {add, a}.
%   oneof([
%     {add, set_element()},
%     {add_all, list(set_element())},
%     {remove, set_element()},
%     {remove_all, list(set_element())}
%     % {reset, {}}
%   ]).

% set_element() ->
%   oneof([a, b]).