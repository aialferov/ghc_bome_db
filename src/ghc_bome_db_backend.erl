-module(ghc_bome_db_backend).

-export([
    load/1, save/2,
    put/3, patch/3, get/3, delete/3
]).

load(FileName) ->
    case file:consult(FileName) of
        {ok, [State]} -> {ok, State};
        {error, enoent} -> {ok, #{}};
        {error, Reason} -> {error, Reason}
    end.

save(FileName, State) ->
    file:write_file(FileName, lists:flatten(io_lib:format("~p.", [State]))).

put(Id, Data, State) ->
    IsModified = maps:is_key(Id, State),
    NewState = maps:put(Id, Data, State),
    case IsModified of
        true -> {ok, {modified, NewState}};
        false -> {ok, {created, NewState}}
    end.

patch(Id, Data, State) ->
    case maps:find(Id, State) of
        {ok, PrevData} -> {ok, maps:put(Id, maps:merge(PrevData, Data), State)};
        error -> {error, not_found}
    end.

get(Id, Options, State) ->
    case maps:find(Id, State) of
        {ok, Data} -> {ok, get(Data, Options)};
        error -> {error, not_found}
    end.

get(Data, Options) ->
    lists:foldl(fun get_apply_option/2, Data, Options).

get_apply_option({filter, DataKeys}, Data) ->
    maps:with(DataKeys, Data).

delete(Id, DataKeys, State) ->
    case maps:find(Id, State) of
        {ok, PrevData} -> {ok, delete(Id, DataKeys, PrevData, State)};
        error -> {error, not_found}
    end.

delete(Id, DataKeys, Data, State) ->
    NewData = maps:without(DataKeys, Data),
    case maps:size(NewData) > 0 of
        true -> maps:put(Id, NewData, State);
        false -> maps:remove(Id, State)
    end.
