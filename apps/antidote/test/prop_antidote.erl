-module(prop_antidote).
% -include_lib("proper/include/proper.hrl").

% -export([command/1, initial_state/0, next_state/3,
%               precondition/2, postcondition/3]).

% %-record(config, {c}).
% % -record(state, {count, node, object}).
% % -define(BUCKET, test_utils:bucket(antidote_bucket)).

% initial_state() ->
%     _Bucket = test_utils:bucket(antidote_bucket),
%     _Type = antidote_crdt_counter_pn,
%     _Key = antidote_key_static_prop_stful,
%     Node = proplists:get_value(node, test_utils:init_prop_single_dc(?MODULE, #{})),
%     ct:pal("initstate node [~p]", [Node]),
%     #{}.
%     % #state{count  = 0, node = Node, object = {Key, Type, Bucket}}.

% %% Picks whether a command should be valid under the current state.
% precondition(_State, {call, _Mod, _Fun, _Args}) -> true.
% %% Given the state `State' *prior* to the call `{call, Mod, Fun, Args}', %% determine whether the result `Res' (coming from the actual system) %% makes sense.
% postcondition(_State, {call, _Mod, _Fun, _Args}, _Res) ->
%     true.
%     % AnitArg = lists:last(Args),
%     % Upd = lists:last(AnitArg),
%     % [{Obj, _, _}] = Upd, 
%     % {ok, [Val], _} = rpc:call(State#state.node, antidote, read_objects, [ignore, [], [Obj]]),
%     % Val =:= State#state.count.

% %% Assuming the postcondition for a call was true, update the model %% accordingly for the test to proceed.
% next_state(State, _Res, {call, _Mod, _Fun, _Args}) ->
%     NewState = State,
%     NewState.

% command(_State) ->
%     % Update = {State#state.object, increment, int()},
%     ct:pal("command print"),
%     oneof([{call, lists, last, [hallo]}]).
%     % oneof([{call, rpc, call, [State#state.node, antidote, update_objects, [ignore, [], [Update]]]}]).
    
% %%%%%%%%%%%%%%%%%%
% %%% PROPERTIES %%%
% %%%%%%%%%%%%%%%%%%


% prop_test_stf() ->
%     ?FORALL(Cmds, commands(?MODULE), 
%         begin
%             ct:pal("[first commands run]"),
%             {History, State, Result} = run_commands(?MODULE, Cmds),
%             % ct:pal("[first commands run[~p]]", [History]),
%             % Object = State#state.object,
%             % Node = State#state.object,
%             % {ok, [Val], _} = rpc:call(Node, antidote, read_objects, [ignore, [], [Object]]),
%             % Update = {Object, increment, -Val},
%             % {ok, _} = rpc:call(Node, antidote, update_objects, [ignore, [], [Update]]),
%             ?WHENFAIL(io:format("History: ~p\nState: ~p\nResult: ~p\n",
%                 [History,State,Result]), aggregate(command_names(Cmds), Result =:= ok))
%         end
%     ).

% prop_test() ->
%     Node = proplists:get_value(node, test_utils:init_prop_single_dc(what, #{})),
%     Bucket = test_utils:bucket(antidote_bucket),
%     Type = antidote_crdt_counter_pn,
%     ?FORALL(Var, {op(), int()}, 
%         begin
%             Key = list_to_atom("antidote_key_static_prop" ++ float_to_list(rand:uniform())), %Used to not use the same Key twice
%             Object = {Key, Type, Bucket},
%             {_, Int} = Var, 
%             Update = {Object, increment, Int},
%             {ok, _} = rpc:call(Node, antidote, update_objects, [ignore, [], [Update]]),
%             {ok, [Val], _} = rpc:call(Node, antidote, read_objects, [ignore, [], [Object]]),
%             Val =:= Int
%         end
%     ).

%%%%%%%%%%%%%%%%%%
%%% HELPERS %%%
%%%%%%%%%%%%%%%%%%

% op() ->
%   oneof([
%     increment,
%     decrement
%   ]).