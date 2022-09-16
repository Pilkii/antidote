-module(prop_transaction_static_parallel).
% -include_lib("proper/include/proper.hrl").

% %% Model Callbacks
% -export([command/1, initial_state/0, next_state/3, doNada/0,
%          precondition/2, postcondition/3]).

% -record(state, {count, nodes, object, clock, updates_done, updates_to_do}).

% %%%%%%%%%%%%%%%%%%
% %%% PROPERTIES %%%
% %%%%%%%%%%%%%%%%%%

% prop_test_parallel() ->
%     test_utils:init_prop_multi_dc(?MODULE, #{}),
%     io:format("hello i am still rnn"),
%     Clusters = proplists:get_value(clusters, test_utils:init_prop_multi_dc(?MODULE, #{})),
%     Nodes = lists:flatten(Clusters),
%     persistent_term:put(nodes, Nodes),
%     io:format("node amount : ~p ~n", [lists:length(Nodes)]),
%     ?FORALL(Cmds, parallel_commands(?MODULE),
%             begin
%                 {History, State, Result} = run_parallel_commands(?MODULE, Cmds),
%                  io:format("result : ~p ~n", [Result]),
%                 ?WHENFAIL(io:format("=======~n"
%                         "Failing command sequence:~n~p~n"
%                         "At state: ~p~n"
%                         "=======~n"
%                         "Result: ~p~n"
%                         "History: ~p~n",
%                         [Cmds,State,Result,History]), 
%                     aggregate(command_names(Cmds), Result =:= ok))
%             end).    



% %%%%%%%%%%%%%
% %%% MODEL %%%
% %%%%%%%%%%%%%
% %% @doc Initial model value at system start. Should be deterministic.
% initial_state() ->
%     Bucket = test_bucket, % test_utils:bucket(antidote_bucket), doesnt work with this
%     Type = antidote_crdt_counter_pn,
%     Key = antidote_key_static_prop_stful,
%     Nodes = persistent_term:get(nodes),
%     Object = {Key, Type, Bucket},
%     Node = oneof(Nodes),
%     UpdatesToDo = integer(2,10),
%     {ok, [Val], Clock} = rpc:call(Node, antidote, read_objects, [ignore, [], [Object]]),
%     State = #state{count = Val, nodes = Nodes, object = Object, clock = Clock, updates_to_do = UpdatesToDo, updates_done = 0},
%     State.

% %% @doc List of possible commands to run against the system
% command(State) ->
%     Update = {State#state.object, op(), val()},
%     Object = State#state.object,
%     Node = oneof(State#state.nodes),
%     io:format("node amount : ~p ~n", [lists:length(State#state.nodes)]),
%     io:format("chosen node : ~p ~n", [Node]),
%     if 
%         State#state.updates_done < State#state.updates_to_do -> 
%             Command = {call, rpc, call, [Node, antidote, update_objects, [State#state.clock, [], [Update]]]};
%         State#state.updates_done =:= State#state.updates_to_do ->
%             F = fun() ->
%                 {ok, [Val], _CommitTime} = rpc:call(Node, antidote, read_objects, [ignore, [], [Object]]),
%                 Val
%             end,
%             Delay = 100,
%             Retry = 360000 div Delay, %wait for max 1 min
%             Command = {call, rpc, call, [Node, time_utils, wait_until_result, [F, 3, Retry, Delay]]};
%         true -> 
%             Command = {call, rpc, call, [Node, prop_transaction_static_parallel, doNada, []]}
%     end,
%     Command.

% %% @doc Determines whether a command should be valid under the
% %% current state.
% precondition(_State, {call, _Mod, _Fun, _Args}) ->
%     true.


% %% @doc Given the state `State' *prior* to the call
% %% `{call, Mod, Fun, Args}', determine whether the result
% %% `Res' (coming from the actual system) makes sense.
% postcondition(_, {call, _Mod, _Fun, [_, _, update_objects, [_, _, [{_, _, _}]]]}, {ok, _}) ->
%     true;
% postcondition(#state{count = Count}, {call, _Mod, _Fun, [_, _, read_objects, _]}, {ok, [Val], _}) ->
%     Count =:= Val;
% postcondition(_, {call, _Mod, _Fun, _Args}, Res) ->
%     io:format("result : ~p ~n", [Res]),
%     false.

% %% @doc Assuming the postcondition for a call was true, update the model
% %% accordingly for the test to proceed.

% next_state(State=#state{count = Count}, {ok, NewClock}, {call, _Mod, _Fun, [_, _, update_objects, [_, _, [{_, increment, Int}]]]}) ->
%     NewState = State#state{count = Count + Int, clock = NewClock},
%     NewState;
% next_state(State=#state{count = Count}, {ok, NewClock}, {call, _Mod, _Fun, [_, _, update_objects, [_, _, [{_, decrement, Int}]]]}) ->
%     NewState = State#state{count = Count - Int, clock = NewClock},
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

% val() ->
%     ?SIZED(Size, resize(100*Size, int())).



% doNada() ->  nichts.