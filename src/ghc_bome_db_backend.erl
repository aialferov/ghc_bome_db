-module(ghc_bome_db_backend).

-export([
    load/1, save/2,

    put/4,
    get/3, get/2,
    delete/3, delete/2
]).

load(FileName) ->
    case file:consult(FileName) of
        {ok, [Data]} -> Data;
        {error, _Reason} -> #{}
    end.

save(FileName, Data) ->
    file:write_file(FileName, lists:flatten(io_lib:format("~p.", [Data]))).

put(User, Type, Value, Data) ->
    UserData = maps:get(User, Data, #{}),
    maps:put(User, maps:put(Type, Value, UserData), Data).

get(User, Type, Data) ->
    maps:get(Type, maps:get(User, Data, #{}), #{}).

get(User, Data) ->
    maps:get(User, Data, #{}).

delete(User, Type, Data) ->
    UserData = maps:get(User, Data, #{}),
    maps:put(User, maps:remove(Type, UserData), Data).

delete(User, Data) ->
    maps:remove(User, Data).
