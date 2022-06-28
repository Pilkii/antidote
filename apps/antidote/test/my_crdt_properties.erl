-module(my_crdt_properties).

% -define(PROPER_NO_TRANS, true).
% -include_lib("proper/include/proper.hrl").



% -export([
%   crdt_satisfies_spec/3,
%   spec_to_partial/1,
%   clock_le/2
% ]).

% -export_type([clocked_operation/0]).



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

% replica() -> oneof([dc1, dc2, dc3]).

% clock_le(A, B) ->
%   lists:all(fun(R) -> maps:get(R, A) =< maps:get(R, B, 0) end, maps:keys(A)).

% -spec crdt_satisfies_spec(atom(), fun(() -> proper_types:raw_type()), fun(([clocked_operation()]) -> term())) -> proper:forall_clause().
% crdt_satisfies_spec(Crdt, OperationGen, Spec) ->
%   ?FORALL(Ops, generateOps(OperationGen),
%         true
%     %   checkSpec(Crdt, Ops, Spec)
%     ).

% % generates a list of operations
% generateOps(OpGen) ->
%   list(
%     % execute operation on a given replica
%     {exec, replica(), OpGen()}
%   ).

% % executes/checks the specification
% checkPartialSpec(Crdt, Ops, Spec) ->
%   % check that the CRDT is registered:
%   true = antidote_crdt:is_type(Crdt),
%   % check that all generated operatiosn are valid:
%   _ = [case Crdt:is_operation(Op) of
%             true -> true;
%             false -> throw({invalid_operation, Op})
%           end || {exec, _, Op} <- Ops],

%   InitialState = maps:from_list(
%     [{Dc, #test_replica_state{state = Crdt:new()}} || Dc <- [dc1, dc2, dc3]]),
%   EndState = execSystem(Crdt, Ops, InitialState),
%   conjunction(
%     [{R,
%       checkSpecEnd(Crdt, Spec, EndState, R)}
%     || R <- maps:keys(EndState)]).

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

% % executes/checks the specification
% checkSpec(Crdt, Ops, Spec) ->
%   checkPartialSpec(Crdt, Ops, spec_to_partial(Spec)).

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