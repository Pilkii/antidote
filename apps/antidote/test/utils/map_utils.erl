-module(map_utils).
-include_lib("proper/include/proper.hrl").

-export([setOp/0, counterOp/0, flagOp/0, registerOp/0, mapOp/2]).
-record(state, {map, node, object, clock}).

setOp() ->
  oneof([
    {add, random_element()},
    {add, simple_set_element()},
    {add_all, list(random_element())},
    {remove, random_element()},
    {remove, simple_set_element()},
    {remove_all, list(random_element())},
    {reset, {}}
  ]).

random_element() ->
  ?SIZED(Size, resize(Size*10, term())).

simple_set_element() ->
    oneof([a, b]).


counterOp() ->
    oneof([
        {increment, ?SIZED(Size, resize(Size, int()))},
        {decrement, ?SIZED(Size, resize(Size, int()))},
        {set, ?SIZED(Size, resize(Size, int()))},
        {reset, {}}
  ]).

flagOp() ->
    oneof([
        {enable, {}},
        {disable,{}},
        {reset, {}}
  ]).

registerOp() ->
    oneof([
        {assign, {term(), non_neg_integer()}},
        {assign, term()},
        %  {assign, any()}, does not work
        {reset, {}}
  ]).



mapOp(State, MapCrdt) -> 
    ?SIZED(Size, mapOp(State, Size, MapCrdt)).

mapOp(State, Size, MapCrdt) ->
    oneof([
        {State#state.object, update, nestedOp(Size, MapCrdt)},
        {State#state.object, update, list(nestedOp(Size, MapCrdt))}
    ]).

recOp(Size, MapCrdt) -> 
    oneof([
        {update, nestedOp(Size, MapCrdt)},
        {update, list(nestedOp(Size, MapCrdt))}
        ]).

%%all of the CRDTs implement the reset operation, because that is necessary for the rrmap
nestedOp(Size, MapCrdt) ->
  oneof(
    [  
    %   {{simple_set_element(), antidote_crdt_set_rw}, map_utils:setOp()},
    %   {{simple_set_element(), antidote_crdt_set_aw}, map_utils:setOp()},
    %   {{simple_set_element(), antidote_crdt_flag_dw}, map_utils:flagOp()},
    %   {{simple_set_element(), antidote_crdt_flag_ew}, map_utils:flagOp()},
    %   {{simple_set_element(), antidote_crdt_register_mv}, map_utils:registerOp()},
      {{simple_set_element(), antidote_crdt_counter_fat}, map_utils:counterOp()}
    ]
    ++
    if
      Size > 1 ->
        [{{term(), MapCrdt}, ?LAZY(recOp(Size div 2, MapCrdt))}];
      true -> []
    end
    ).

