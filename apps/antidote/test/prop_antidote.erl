-module(prop_antidote).
% -include_lib("proper/include/proper.hrl").

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

