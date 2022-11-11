-module(model_adjustments_crdts).

-export([isEqual/3, turnIntoErlangType/2, adjustModel/3]).

crdtIsSet(Crdt) ->
    case Crdt of
        antidote_crdt_set_aw -> true;
        antidote_crdt_set_go -> true;
        antidote_crdt_set_rw -> true;
        _ -> false
    end.

crdtIsNumberType(Crdt) ->
    case Crdt of
        antidote_crdt_counter_pn -> true;
        antidote_crdt_counter_b -> true;
        antidote_crdt_counter_fat -> true;
        _ -> false
    end.

crdtIsFlag(Crdt) ->
    case Crdt of
        antidote_crdt_flag_ew -> true;
        antidote_crdt_flag_dw -> true;
        _ -> false
    end.

crdtIsMap(Crdt) ->
    case Crdt of
        antidote_crdt_map_go -> true;
        antidote_crdt_map_rr -> true;
        _ -> false
    end.
crdtIsRegister(Crdt) ->
    case Crdt of
        antidote_crdt_register_lww -> true;
        antidote_crdt_register_mv -> true;
        _ -> false
    end.

turnIntoErlangType(Instance, Crdt) ->
    case {crdtIsMap(Crdt), crdtIsSet(Crdt), crdtIsNumberType(Crdt), crdtIsFlag(Crdt), crdtIsRegister(Crdt)} of
        {true, _, _, _, _} -> 
            mapFromList(Instance);
        {_, true, _, _, _} ->  
            sets:from_list(Instance);
        {_, _, true, _, _} -> 
            Instance;
        {_, _, _, true, _} -> 
            Instance;
        {_, _, _, _, true} -> 
            Instance;
        _ -> error
    end.

%%% Sets %%%
adjustSet(_, {reset, {}}) ->
    sets:new();
adjustSet(Set, {add, Elem}) ->
    NewSet = sets:add_element(Elem, Set),
    NewSet;
adjustSet(Set, {remove, Elem}) ->
    NewSet = sets:del_element(Elem, Set),
    NewSet;
adjustSet(Set, {add_all, ElemList}) ->
    Set2 = sets:from_list(ElemList),
    NewSet = sets:union(Set, Set2),
    NewSet;
adjustSet(Set, {remove_all, ElemList}) ->
    Set2 = sets:from_list(ElemList),
    NewSet = sets:subtract(Set, Set2),
    NewSet.


%%% NumberTypes %%%
adjustNumberType(Count, []) ->
    Count;
adjustNumberType(Count, {increment, Int}) ->
    Count + Int;
adjustNumberType(Count, {decrement, Int}) ->
    Count - Int;
adjustNumberType(_Count, {set, Int}) ->
    Int;
adjustNumberType(_Count, {reset, {}}) ->
    0;
adjustNumberType(Count, [Op | Ops]) ->
    adjustNumberType(adjustNumberType(Count, Ops), Op).

%%% Register %%%
adjustRegister(_, {reset, {}}, antidote_crdt_register_mv) ->
    [];
adjustRegister(_, {assign, Value}, antidote_crdt_register_mv) ->
    [Value];
adjustRegister(_, {assign, Value}, antidote_crdt_register_lww) ->
    Value.

%%% Flags %%%
adjustFlag(_, {enable, {}}) ->
    true;
adjustFlag(_, {disable, {}}) ->
    false;
adjustFlag(_, {reset, {}}) ->
    false.

%%% Maps %%%
%%% 

adjustMap(Map, {_, []}, _) ->
    io:format("EMPTY MAP ADJUSTED --------------"),
    Map;

adjustMap(Map, {remove, [H | T]}, MapCrdt) ->
    NewMap = adjustMap(Map, {remove, H}, MapCrdt),
    adjustMap(NewMap, {remove, T}, MapCrdt);

adjustMap(Map, {remove, {Key, Crdt}}, _MapCrdt) ->
    NewMap = maps:remove({Key, Crdt}, Map),
    NewMap;

adjustMap(_Map, {reset, {}}, _MapCrdt) ->
    maps:new();


adjustMap(Map, {update, [H | T]}, MapCrdt) ->
    NewMap = adjustMap(Map, {update, H}, MapCrdt),
    adjustMap(NewMap, {update, T}, MapCrdt);

adjustMap(Map, {update, {{Key, Crdt}, Op}}, MapCrdt) ->
    case {crdtIsMap(Crdt), crdtIsSet(Crdt), crdtIsNumberType(Crdt), crdtIsFlag(Crdt), crdtIsRegister(Crdt)} of
        {true, _, _, _, _} ->  
            F = fun(X, Y) -> adjustMap(X, Y, Crdt) end,
            updateMap(Map, {update, {{Key, Crdt}, Op}}, MapCrdt, #{}, F);
        {_, true, _, _, _} ->  
            updateMap(Map, {update, {{Key, Crdt}, Op}}, MapCrdt, sets:new(), fun adjustSet/2);
        {_, _, true, _, _} -> 
            updateMap(Map, {update, {{Key, Crdt}, Op}}, MapCrdt, 0, fun adjustNumberType/2);
        {_, _, _, true, _} -> 
            updateMap(Map, {update, {{Key, Crdt}, Op}}, MapCrdt, false, fun adjustFlag/2);
        {_, _, _, _, true} -> 
            F = fun(X, Y) -> adjustRegister(X, Y, Crdt) end,
            if 
                Crdt =:= antidote_crdt_register_mv -> updateMap(Map, {update, {{Key, Crdt}, Op}}, MapCrdt, [], F);
                Crdt =:= antidote_crdt_register_lww -> updateMap(Map, {update, {{Key, Crdt}, Op}}, MapCrdt, <<>>, F)
            end;
        _ -> error
    end;


adjustMap(Map, {batch, {Update, Removes}}, MapCrdt) ->
    NewMap = adjustMap(Map, {update, Update}, MapCrdt),
    adjustMap(NewMap, {remove, Removes}, MapCrdt).

updateMap(Map, {update, {{Key, Crdt}, Op}}, MapCrdt, InitValue, AdjustFunction) ->
    FullKey = {Key, Crdt},
    InnerInstance = maps:get(FullKey, Map, InitValue),
    NewInnerInstance = AdjustFunction(InnerInstance, Op),
    RemovesInit = mapRemovesInitValues(MapCrdt),
    if 
        RemovesInit and (NewInnerInstance =:= InitValue) -> NewMap = maps:remove(FullKey, Map);
        true -> NewMap = maps:put(FullKey, NewInnerInstance, Map)
    end,
    NewMap.

%%% Map Helpers %%%
mapRemovesInitValues(MapCrdt) ->
    MapCrdt =:= antidote_crdt_map_rr.

isEqual(Instance, InstanceFromAntidote, Crdt) ->
    case {crdtIsMap(Crdt), crdtIsSet(Crdt), crdtIsNumberType(Crdt), crdtIsFlag(Crdt), crdtIsRegister(Crdt)} of
         {true, _, _, _, _} -> 
            X =  mapFromList(InstanceFromAntidote),
            if 
                not (X =:= Instance) -> io:format("Comparison [~p] [~p]", [X, Instance]),
                                        X =:= Instance;
                true -> X =:= Instance
            end;
            % Map = maps:from_list(InstanceFromAntidote),
            % Fun = fun(K, V, Bool) -> 
            %     {_, Type} = K,
            %     MapContainsKey = maps:is_key(K, Instance),
            %     Value = maps:get(K, Instance, ignore), %%%it is important that we use andalso here. Because get could throw errors in case Instance does not contain key
            %     Bool andalso MapContainsKey andalso isEqual(Value, V, Type)
            %     end,
            % MapsHaveEqualSize = maps:size(Instance) =:= maps:size(Map), %%to check that one map is not just contained in the other
            % maps:fold(Fun, MapsHaveEqualSize, Map);
        {_, true, _, _, _} ->  
            Set = sets:from_list(InstanceFromAntidote),
            sets:is_subset(Instance, Set) and sets:is_subset(Set, Instance);
        {_, _, true, _, _} -> 
            Instance =:= InstanceFromAntidote;
        {_, _, _, true, _} -> 
            Instance =:= InstanceFromAntidote;
        {_, _, _, _, true} -> 
            Instance =:= InstanceFromAntidote;
        _ -> error
    end.


adjustModel(Model, Operation, Crdt) ->
    case {crdtIsMap(Crdt), crdtIsSet(Crdt), crdtIsNumberType(Crdt), crdtIsFlag(Crdt), crdtIsRegister(Crdt)} of
         {true, _, _, _, _} -> 
            adjustMap(Model, Operation, Crdt);
        {_, true, _, _, _} ->  
            adjustSet(Model, Operation);
        {_, _, true, _, _} -> 
            adjustNumberType(Model, Operation);
        {_, _, _, true, _} -> 
            adjustFlag(Model, Operation);
        {_, _, _, _, true} -> 
            adjustRegister(Model, Operation, Crdt);
        _ -> error
    end.
%%% also turns all the instances inside the map to appropriate types
mapFromList(Map) ->
    Fun = fun(K, V, M) -> 
                {_, Type} = K,
                Value = turnIntoErlangType(V, Type),
                NewMap = maps:put(K, Value, M),
                NewMap
                end,
    maps:fold(Fun, #{}, maps:from_list(Map)).
