-module(ghc_bome_db_backend_test).

-include_lib("eunit/include/eunit.hrl").

-define(M, ghc_bome_db_backend).

put_test() ->
    ?assertEqual(?M:put(
        "user", #{"type" => "value"}, #{}),
        {ok, {created, #{"user" => #{"type" => "value"}}}}
    ),
    ?assertEqual(?M:put(
        "user", #{"type" => "value"}, #{"user" => #{"type" => "value"}}),
        {ok, {modified, #{"user" => #{"type" => "value"}}}}
    ),
    ?assertEqual(?M:put(
        "u", #{"t1" => "v1"}, #{"u" => #{"t" => "v"}}),
        {ok, {modified, #{"u" => #{"t1" => "v1"}}}}
    ),
    ?assertEqual(?M:put(
        "u", #{"t2" => "v2"}, #{"u" => #{"t" => "v", "t1" => "v1"}}),
        {ok, {modified, #{"u" => #{"t2" => "v2"}}}}
    ),
    ?assertEqual(?M:put(
        "u1", #{"t" => "v"}, #{"u" => #{"t" => "v", "t1" => "v1"}}),
        {ok, {created, #{"u" => #{"t" => "v", "t1" => "v1"},
                         "u1" => #{"t" => "v"}}}}
    ).

patch_test() ->
    ?assertEqual(?M:patch(
        "user", #{"type" => "value"}, #{}),
        {error, not_found}
    ),
    ?assertEqual(?M:patch(
        "user", #{"type" => "new_value"}, #{"user" => #{"type" => "value"}}),
        {ok, #{"user" => #{"type" => "new_value"}}}
    ),
    ?assertEqual(?M:patch(
        "u", #{"t1" => "v1"}, #{"u" => #{"t" => "v"}}),
        {ok, #{"u" => #{"t" => "v", "t1" => "v1"}}}
    ),
    ?assertEqual(?M:patch(
        "u", #{"t2" => "v2"}, #{"u" => #{"t" => "v", "t1" => "v1"}}),
        {ok, #{"u" => #{"t" => "v", "t1" => "v1", "t2" => "v2"}}}
    ),
    ?assertEqual(?M:patch(
        "u1", #{"t" => "v"}, #{"u" => #{"t" => "v", "t1" => "v1"}}),
        {error, not_found}
    ).

get_test() ->
    ?assertEqual(?M:get(
        "user", [], #{}),
        {error, not_found}
    ),
    ?assertEqual(?M:get(
        "user", [{filter, ["type"]}], #{}),
        {error, not_found}
    ),
    ?assertEqual(?M:get(
        "u", [], #{"u" => #{"t1" => "v1", "t2" => "v2"}}),
        {ok, #{"t1" => "v1", "t2" => "v2"}}
    ),
    ?assertEqual(?M:get(
        "u", [{filter, ["t2"]}], #{"u" => #{"t1" => "v1", "t2" => "v2"}}),
        {ok, #{"t2" => "v2"}}
    ),
    ?assertEqual(?M:get(
        "u", [{filter, ["t3"]}], #{"u" => #{"t1" => "v1", "t2" => "v2"}}),
        {ok, #{}}
    ),
    ?assertEqual(?M:get(
        "u1", [], #{"u" => #{"t" => "v"}}),
        {error, not_found}
    ),
    ?assertEqual(?M:get(
        "u1", [{filter, ["t"]}], #{"u" => #{"t" => "v"}}),
        {error, not_found}
    ).

delete_test() -> 
    ?assertEqual(?M:delete(
        "user", #{}, #{}),
        {error, not_found}
    ),
    ?assertEqual(?M:delete(
        "user", ["type"], #{}),
        {error, not_found}
    ),
    ?assertEqual(?M:delete(
        "u", ["t"], #{"u" => #{"t" => "v"}}),
        {ok, #{}}
    ),
    ?assertEqual(?M:delete(
        "u", ["t1"], #{"u" => #{"t1" => "v1", "t2" => "v2"}}),
        {ok, #{"u" => #{"t2" => "v2"}}}
    ),
    ?assertEqual(?M:delete(
        "u", ["t1", "t2"], #{"u" => #{"t1" => "v1", "t2" => "v2"}}),
        {ok, #{}}
    ),
    ?assertEqual(?M:delete(
        "u1", [], #{"u" => #{"t1" => "v1", "t2" => "v2"}}),
        {error, not_found}
    ),
    ?assertEqual(?M:delete(
        "u1", ["t1"], #{"u" => #{"t1" => "v1", "t2" => "v2"}}),
        {error, not_found}
    ),
    ?assertEqual(?M:delete(
        "u1", ["t1"], #{"u" => #{"t1" => "v1", "t2" => "v2"},
                        "u1" => #{"t1" => "v1", "t2" => "v2"}}),
        {ok, #{"u" => #{"t1" => "v1", "t2" => "v2"},
               "u1" => #{"t2" => "v2"}}}
    ).
