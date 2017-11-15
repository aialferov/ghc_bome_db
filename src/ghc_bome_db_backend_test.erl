-module(ghc_bome_db_backend_test).

-include_lib("eunit/include/eunit.hrl").

-define(M, ghc_bome_db_backend).

put_test() ->
    ?assertEqual(?M:put(
        "user", "type", "value", #{}),
        #{"user" => #{"type" => "value"}}
    ),
    ?assertEqual(?M:put(
        "user", "type", "value", #{"user" => #{"type" => "value"}}),
        #{"user" => #{"type" => "value"}}
    ),
    ?assertEqual(?M:put(
        "u", "t1", "v1", #{"u" => #{"t" => "v"}}),
        #{"u" => #{"t" => "v", "t1" => "v1"}}
    ),
    ?assertEqual(?M:put(
        "u1", "t", "v", #{"u" => #{"t" => "v", "t1" => "v1"}}),
        #{"u" => #{"t" => "v", "t1" => "v1"}, "u1" => #{"t" => "v"}}
    ).

get_test() ->
    ?assertEqual(?M:get("user", #{}), #{}),
    ?assertEqual(?M:get("user", "type", #{}), #{}),
    ?assertEqual(?M:get("u", #{"u" => #{"t" => "v"}}), #{"t" => "v"}),
    ?assertEqual(?M:get("u", "t", #{"u" => #{"t" => "v"}}), #{"t" => "v"}),
    ?assertEqual(?M:get(
        "u", #{"u" => #{"t" => "v", "t1" => "v1"}}),
        #{"t" => "v", "t1" => "v1"}
    ),
    ?assertEqual(?M:get(
        "u", "t1", #{"u" => #{"t" => "v", "t1" => "v1"}}),
        #{"t1" => "v1"}
    ).

delete_test() -> 
    ?assertEqual(?M:delete("user", #{}), #{}),
    ?assertEqual(?M:delete("user", "type", #{}), #{}),
    ?assertEqual(?M:delete("u", #{"u" => #{"t" => "v"}}), #{}),
    ?assertEqual(?M:delete("u", "t", #{"u" => #{"t" => "v"}}), #{}),
    ?assertEqual(?M:delete("u", #{"u" => #{"t" => "v", "t1" => "v1"}}), #{}),
    ?assertEqual(?M:delete(
        "u", "t1", #{"u" => #{"t" => "v", "t1" => "v1"}}),
        #{"u" => #{"t" => "v"}}
    ).
