-module(prop_antidote).
-include_lib("proper/include/proper.hrl").

-export([command/1, initial_state/0, next_state/3,
              precondition/2, postcondition/3]).

-record(config, {c}).
% -define(BUCKET, test_utils:bucket(antidote_bucket)).

initial_state() ->
    {}.
    % ct:pal("[print me]"),
    % #config{c=test_utils:init_prop_single_dc(?MODULE, {})}.
    % #state{users  = [],
    %        rented = []}.

%% Picks whether a command should be valid under the current state.
precondition(_State, {call, _Mod, _Fun, _Args}) -> true.
%% Given the state `State' *prior* to the call `{call, Mod, Fun, Args}', %% determine whether the result `Res' (coming from the actual system) %% makes sense.
postcondition(_State, {call, _Mod, _Fun, _Args}, _Res) ->
true.

%% Assuming the postcondition for a call was true, update the model %% accordingly for the test to proceed.
next_state(State, _Res, {call, _Mod, _Fun, _Args}) ->
    NewState = State,
    NewState.

command(_State) ->
    [].
    
%%%%%%%%%%%%%%%%%%
%%% PROPERTIES %%%
%%%%%%%%%%%%%%%%%%

prop_test() ->
    _ = test_utils:init_prop_single_dc(?MODULE, {}),
    % _ = proplists:get_value(node, test_utils:init_prop_single_dc(what, #{})),
    % Bucket = test_utils:bucket(antidote_bucket),
    % Key = antidote_key_static1,
    % Type = antidote_crdt_counter_pn,
    % Object = {Key, Type, Bucket},
    ?FORALL(_, int(), 
        begin
            % Update = {Object, increment, Int},
            % {ok, _} = rpc:call(Node, antidote, update_objects, [ignore, [], [Update]]),
            % {ok, [Val], _} = rpc:call(Node, antidote, read_objects, [ignore, [], [Object]]),
            % Val =:= Int
            true
        end
    ).

% -define(BUCKET, test_utils:bucket(antidote_bucket)).

% -export([
%     prop_static_txn_single_object/0
% ]).


% prop_static_txn_single_object() ->
%     Bucket = ?BUCKET,
%     Node = 'antidote@127.0.0.1',
%     Key = antidote_key_static_pb,
%     Type = antidote_crdt_counter_pn,
%     Object = {Key, Type, Bucket},
%     Update = {Object, increment, 1},
%     ?FORALL(Int, int(), 
%         begin
%             _ = {Object, increment, Int},
%             {ok, _} = rpc:call(Node, antidote, update_objects, [ignore, [], [Update]]),
%             {ok, [Val], _} = rpc:call(Node, antidote, read_objects, [ignore, [], [Object]]),
%             Val =:= 1
%         end
%     ).

