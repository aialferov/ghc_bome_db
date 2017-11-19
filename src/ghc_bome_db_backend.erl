-module(ghc_bome_db_backend).

-export([
    load/1, save/2,
    put/3, patch/3, get/3, delete/3
]).

load(FileName) ->
    case file:consult(FileName) of
        {ok, [Data]} -> {ok, Data};
        {error, enoent} -> {ok, #{}};
        {error, Reason} -> {error, Reason}
    end.

save(FileName, Data) ->
    file:write_file(FileName, lists:flatten(io_lib:format("~p.", [Data]))).

put(UserId, Metrics, Data) ->
    IsModified = maps:is_key(UserId, Data),
    NewData = maps:put(UserId, Metrics, Data),
    case IsModified of
        true -> {ok, {modified, NewData}};
        false -> {ok, {created, NewData}}
    end.

patch(UserId, Metrics, Data) ->
    case maps:find(UserId, Data) of
        {ok, PrevMetrics} ->
            {ok, maps:put(UserId, maps:merge(PrevMetrics, Metrics), Data)};
        error -> {error, not_found}
    end.

get(UserId, Options, Data) ->
    case maps:find(UserId, Data) of
        {ok, Metrics} -> {ok, get(Metrics, Options)};
        error -> {error, not_found}
    end.

get(Metrics, Options) ->
    lists:foldl(fun get_apply_option/2, Metrics, Options).

get_apply_option({filter, MetricNames}, Data) ->
    maps:with(MetricNames, Data).

delete(UserId, MetricNames, Data) ->
    case maps:find(UserId, Data) of
        {ok, Metrics} -> {ok, delete(UserId, MetricNames, Metrics, Data)};
        error -> {error, not_found}
    end.

delete(UserId, MetricNames, Metrics, Data) ->
    NewMetrics = maps:without(MetricNames, Metrics),
    case maps:size(NewMetrics) > 0 of
        true -> maps:put(UserId, NewMetrics, Data);
        false -> maps:remove(UserId, Data)
    end.
