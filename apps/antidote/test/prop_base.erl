-module(prop_base).
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
                % actual_system:start_link(),
                {History, State, Result} = run_commands(?MODULE, Cmds),
                ?WHENFAIL(io:format("History: ~p\nState: ~p\nResult: ~p\n",
                                    [History,State,Result]),
                          aggregate(command_names(Cmds), Result =:= ok))
            end).

%%%%%%%%%%%%%
%%% MODEL %%%
%%%%%%%%%%%%%
%% @doc Initial model value at system start. Should be deterministic.
initial_state() ->
    Bucket = test_utils:bucket(antidote_bucket),
    Type = antidote_crdt_counter_pn,
    Key = antidote_key_static_prop_stful,
    Node = proplists:get_value(node, test_utils:init_prop_single_dc(?MODULE, #{})),
    Object = {Key, Type, Bucket},
    {ok, [Val], _} = rpc:call(Node, antidote, read_objects, [ignore, [], [Object]]),
    #state{count = Val, node = Node, object = Object}.

%% @doc List of possible commands to run against the system
command(State) ->
    Update = {State#state.object, increment, int()},
    Node = State#state.node,
    % rpc:call(Node, antidote, update_objects, [ignore, [], [Update]]),
    oneof([
        {call, rpc, call, [Node, antidote, update_objects, [ignore, [], [Update]]]}
    ]).
    % oneof([
    %     {call, lists, last, [[term()]]}
    % ]).

%% @doc Determines whether a command should be valid under the
%% current state.
precondition(_State, {call, _Mod, _Fun, _Args}) ->
    true.

%% @doc Given the state `State' *prior* to the call
%% `{call, Mod, Fun, Args}', determine whether the result
%% `Res' (coming from the actual system) makes sense.
postcondition(State, {call, _Mod, _Fun, _Args}, _Res) ->
    {ok, [Val], _} = rpc:call(State#state.node, antidote, read_objects, [ignore, [], [State#state.object]]),
    B = State#state.count =:= Val,
    ct:pal("[~p]", [B]),
    ct:pal("Val [~p]", [Val]),
    ct:pal("Count [~p]", [State#state.count]),
    true.

%% @doc Assuming the postcondition for a call was true, update the model
%% accordingly for the test to proceed.
next_state(State, _Res, {call, _Mod, _Fun, Args}) ->
    FunArgs = lists:last(Args),
    [Update] = lists:last(FunArgs),
    {_, _, Int} = Update,
    NewState = State#state{count = State#state.count + Int},
    NewState.
