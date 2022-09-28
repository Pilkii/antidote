-module(prop_transaction_static_parallel).
-include_lib("proper/include/proper.hrl").

%% Model Callbacks
-export([command/1, initial_state/0, next_state/3, update_command/5,
         precondition/2, postcondition/3]).

-record(state, {count, nodes, object, clock, updates_done, updates_to_do}).

%%%%%%%%%%%%%%%%%%
%%% PROPERTIES %%%
%%%%%%%%%%%%%%%%%%

prop_test_parallel() ->
    % test_utils:init_prop_multi_dc(?MODULE, #{}),
    io:format("hello i am still rnn"),
    % Clusters = proplists:get_value(clusters, test_utils:init_prop_multi_dc(?MODULE, #{})),
    % Nodes = lists:flatten(Clusters),
    Nodes = proplists:get_value(nodes, test_utils:init_prop_multi_dc(?MODULE, #{})),
    persistent_term:put(nodes, Nodes),
    ?FORALL(Cmds, commands(?MODULE),
            begin
                {History, State, Result} = run_commands(?MODULE, Cmds),
                 io:format("result : ~p ~n", [Result]),
                ?WHENFAIL(io:format("=======~n"
                        "Failing command sequence:~n~p~n"
                        "At state: ~p~n"
                        "=======~n"
                        "Result: ~p~n"
                        "History: ~p~n",
                        [Cmds,State,Result,History]), 
                    aggregate(command_names(Cmds), Result =:= ok))
            end).    



%%%%%%%%%%%%%
%%% MODEL %%%
%%%%%%%%%%%%%
%% @doc Initial model value at system start. Should be deterministic.
initial_state() ->
    Bucket = test_bucket, % test_utils:bucket(antidote_bucket), doesnt work with this
    Type = antidote_crdt_counter_pn,
    Key = antidote_key_static_prop_stful,
    Nodes = persistent_term:get(nodes),
    Object = {Key, Type, Bucket},
    [Node1, _Node2] = Nodes,
    {ok, [Val], Clock} = rpc:call(Node1, antidote, read_objects, [ignore, [], [Object]]),
    io:format("init val: ~p ~n  ", [Val]),
    State = #state{count = Val, nodes = Nodes, object = Object, clock = Clock},
    State.

%% @doc List of possible commands to run against the system
command(State) ->
    Nodes = State#state.nodes,
    [Node1, _] = Nodes, 
    Updates = list({oneof(Nodes), {State#state.object, op(), val()}}),
    Object = State#state.object,
    {call, prop_transaction_static_parallel, update_command, [State#state.clock, Updates, Object, Node1, State#state.count]}.

%% @doc Determines whether a command should be valid under the
%% current state.
precondition(_State, {call, _Mod, _Fun, _Args}) ->
    true.


%% @doc Given the state `State' *prior* to the call
%% `{call, Mod, Fun, Args}', determine whether the result
%% `Res' (coming from the actual system) makes sense.
postcondition(_, {call, _Mod, update_command, _}, {ok, _Clock, _NewCounter}) ->
    true;
postcondition(_, {call, _Mod, _Fun, _Args}, _Res) ->
    false.

%% @doc Assuming the postcondition for a call was true, update the model
%% accordingly for the test to proceed.


next_state(State, {ok, Clock, NewCounter}, {call, _Mod, update_command, _}) ->
    NewState = State#state{count = NewCounter, clock = Clock},
    NewState;

next_state(State, _Res, {call, _Mod, _Fun, _}) ->
    State.

%%%%%%%%%%%%%%%%%%
%%%%% HELPERS %%%%
%%%%%%%%%%%%%%%%%%



update_command(Clock, Args, Object, Node, Counter) ->
    {ok, NewClock} = update_command(Args, Clock),
    Updates = [{Op, Int} || {_, {_, Op, Int}} <- Args],
    io:format("Updates: ~p ~n  ", [Updates]),
    NewCounter = prop_transaction_static:adjustCounter(Counter, Updates),
    io:format("NewCounter: : ~p Oldcounter : ~p ~n", [NewCounter, Counter]),
    F = fun() ->
                {ok, [Val], _CommitTime} = rpc:call(Node, antidote, read_objects, [ignore, [], [Object]]),
                io:format("read newcounter: ~p~n  val: ~p~n", [NewCounter, Val]),
                Val
            end,
    Delay = 1000,
    Retry = 360000 div Delay, %wait for max 1 min
    io:format("running"),
    ok = time_utils:wait_until_result(F, NewCounter, Retry, Delay),
    {ok, NewClock, NewCounter}.

update_command([], Clock) ->
    {ok, Clock};

update_command([Arg | Args], _Clock) ->
    io:format("update args: ~p~n",[Arg]),
    {Node, Update} = Arg,
    {ok, NewClock} = rpc:call(Node, antidote, update_objects, [ignore, [], [Update]]),
    io:format("update return: ~p~n",[Arg]),
    update_command(Args, NewClock).

%%%%%%%%%%%%%%%%%%%%%
%%%%% GENERATORS %%%%
%%%%%%%%%%%%%%%%%%%%%


op() ->
    oneof([
        increment,
        decrement
        ]).

val() ->
    ?SIZED(Size, resize(100*Size, int())).

