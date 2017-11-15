-module(ghc_bome_db_backend).

-export([
    load/1, save/2,

    put/4,
    get/3, get/2,
    delete/3, delete/2
]).

load(FileName) ->
    case file:consult(FileName) of
        {ok, [Data]} -> {ok, Data};
        {error, enoent} -> {ok, #{}};
        {error, Reason} -> {error, Reason}
    end.

save(FileName, Data) ->
    ok = file:write_file(FileName, lists:flatten(io_lib:format("~p.", [Data]))).

put(User, Type, Value, Data) ->
    UserData = maps:get(User, Data, #{}),
    maps:put(User, maps:put(Type, Value, UserData), Data).

get(User, Type, Data) ->
    case maps:get(Type, maps:get(User, Data, #{}), #{}) of
        #{} -> #{};
        Value -> #{Type => Value}
    end.

get(User, Data) ->
    maps:get(User, Data, #{}).

delete(User, Type, Data) ->
    UserData = maps:remove(Type, maps:get(User, Data, #{})),
    case maps:size(UserData) > 0 of
        true -> maps:put(User, UserData, Data);
        false -> maps:remove(User, Data)
    end.

delete(User, Data) ->
    maps:remove(User, Data).
