-module(prop_one).
% -define(PROPER_NO_TRANS, true).
% -include_lib("proper/include/proper.hrl").

% -export([prop_static_txn_single_object/0, op/0, spec/1, prop_static_txn_multi_objects/0]).

% -type clock() :: #{replica() => non_neg_integer()}.
% -type clocked_operation() :: {Clock :: clock(), Operation :: any()}.
% -type clocked_effect() :: {Clock :: clock(), Effect :: any()}.
% -type replica() :: dc1 | dc2 | dc3.

% -record(test_replica_state, {
%   state :: any(),
%   clock = #{} :: clock(),
%   operations = [] :: [clocked_operation()],
%   downstreamOps = [] :: [clocked_effect()]
% }).

% -type test_replica_state() :: #test_replica_state{}.
% -type test_state() :: #{replica() => test_replica_state()}.
% -type test_operation() :: {pull, replica(), replica()} | {exec, replica(), any()}.


% prop_static_txn_single_object() ->
%   ?FORALL(Ops, list(), 
%       begin 
%         {ok, TxId} = antidote:start_transaction(ignore, []),
%         antidote:update_objects([{ {counter_key, antidote_crdt_counter_b, test_bucket}, increment, {10, client1}}], TxId),
%         ok =:= antidote:commit_transaction(TxId)
%       end
%     )
    % Crdt = antidote_crdt_counter_pn,
    % ?FORALL(Ops, list({exec, dc1, op()}),
    %     begin
    %         InitialState = maps:from_list(
    %         [{dc1, #test_replica_state{state = Crdt:new()}}]),
    %         EndState = execSystem(Crdt, Ops, InitialState),
    %         conjunction(
    %         [{R,checkSpecEnd(Crdt, spec_to_partial(fun spec/1), EndState, R)} || R <- maps:keys(EndState)])
    %     end
    %   ).

% prop_static_txn_multi_objects() ->
%     Crdt = antidote_crdt_counter_pn,
%     Keys = [dc1, dc2, dc3],
%     ?FORALL(Ops, list({exec, replica(), op()}),
%         begin 
%             InitialState = maps:from_list(
%             [{Key, #test_replica_state{state = Crdt:new()}} || Key <- Keys]),
%             EndState = execSystem(Crdt, Ops, InitialState),
%             conjunction(
%             [{R,checkSpecEnd(Crdt, spec_to_partial(fun spec/1), EndState, R)} || R <- maps:keys(EndState)])
%         end
%       ).

% % interactive_txn_abort(Config) ->
% %     Crdt = antidote_crdt_counter_pn,
% %     ?FORALL(Ops, {exec, dc1, op()),
% %         InitialState = #test_replica_state{state = Crdt:new()},
% %       ).
% %     Bucket = ?BUCKET,
% %     Node = proplists:get_value(node, Config),
% %     Type = antidote_crdt_counter_pn,
% %     Key = antidote_int_abort_m1,
% %     Object = {Key, Type, Bucket},
% %     Update = {Object, increment, 1},
% %     {ok, TxId} = rpc:call(Node, antidote, start_transaction, [ignore, []]),
% %     ok = rpc:call(Node, antidote, update_objects, [[Update], TxId]),
% %     ok = rpc:call(Node, antidote, abort_transaction, [TxId]), % must abort successfully

% %     {ok, TxId2} = rpc:call(Node, antidote, start_transaction, [ignore, []]),
% %     %% read object
% %     {ok, Res} = rpc:call(Node, antidote, read_objects, [[Object], TxId2]),
% %     {ok, _} = rpc:call(Node, antidote, commit_transaction, [TxId2]).


% replica() -> oneof([dc1, dc2, dc3]).

% op() ->
%   oneof([
%     increment,
%     decrement,
%     {increment, integer()},
%     {decrement, integer()}
%   ]).

% spec(Operations) ->
%   lists:sum([X || {_, {increment, X}} <- Operations])
%     + lists:sum([1 || {_, increment} <- Operations])
%     - lists:sum([X || {_, {decrement, X}} <- Operations])
%     - lists:sum([1 || {_, decrement} <- Operations]).

% -spec execSystem(atom(), [test_operation()], test_state()) -> test_state().
% execSystem(_Crdt, [], State) ->
%   State;

% execSystem(Crdt, [{exec, Replica, Op}|RemainingOps], State) ->
%   ReplicaState = maps:get(Replica, State),
%   CrdtState = ReplicaState#test_replica_state.state,
%   CrdtStateForDownstream =
%     case Crdt:require_state_downstream(Op) of
%       true -> CrdtState;
%       false -> no_state
%     end,
%   {ok, Effect} = Crdt:downstream(Op, CrdtStateForDownstream),
%   {ok, NewCrdtState} = Crdt:update(Effect, CrdtState),

%   ReplicaClock = ReplicaState#test_replica_state.clock,
%   NewReplicaClock = ReplicaClock#{Replica => maps:get(Replica, ReplicaClock, 0) + 1},

%   NewReplicaState = ReplicaState#test_replica_state{
%     state = NewCrdtState,
%     clock = NewReplicaClock,
%     operations = ReplicaState#test_replica_state.operations ++ [{NewReplicaClock, Op}],
%     downstreamOps = ReplicaState#test_replica_state.downstreamOps ++ [{NewReplicaClock, Effect}]
%   },
%   NewState = State#{Replica => NewReplicaState},
%   execSystem(Crdt, RemainingOps, NewState).

% checkSpecEnd(Crdt, Spec, EndState, R) ->
%   RState = maps:get(R, EndState),
%   RClock = RState#test_replica_state.clock,
%   RValue = Crdt:value(RState#test_replica_state.state),

%   % get the visible operations:
%   VisibleOperations = [{Clock, Op} ||
%     Replica <- maps:keys(EndState),
%     {Clock, Op} <- (maps:get(Replica, EndState))#test_replica_state.operations,
%     clock_le(Clock, RClock)],

%   ?WHENFAIL(
%     begin
%       io:format("Checking value on ~p~n", [R])
%     end,
%     Spec(VisibleOperations, RValue)
%   ).


% -spec spec_to_partial(fun(([clocked_operation()]) -> term())) -> fun(([clocked_operation()], term()) -> proper:test()).
% spec_to_partial(Spec) ->
%   fun(Operations, RValue) ->
%     SpecValue = Spec(Operations),
%     ?WHENFAIL(
%       begin
%         io:format("Expected value: ~p~n", [SpecValue]),
%         io:format("Actual value  : ~p~n", [RValue])
%       end,
%       SpecValue == RValue
%     )
%   end.

% clock_le(A, B) ->
%   lists:all(fun(R) -> maps:get(R, A) =< maps:get(R, B, 0) end, maps:keys(A)).