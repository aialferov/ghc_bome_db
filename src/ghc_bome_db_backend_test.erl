-module(ghc_bome_db_backend_test).

-include_lib("eunit/include/eunit.hrl").

-define(M, ghc_bome_db_backend).

put_test() ->
    ?assertEqual(?M:put(
        "user_id", #{"metric_name" => "metric_value"}, #{}),
        {ok, {created, #{"user_id" => #{"metric_name" => "metric_value"}}}}
    ),
    ?assertEqual(?M:put(
            "user_id", #{"metric_name" => "metric_value"},
            #{"user_id" => #{"metric_name" => "metric_value"}}
        ),
        {ok, {modified, #{"user_id" => #{"metric_name" => "metric_value"}}}}
    ),
    ?assertEqual(?M:put(
        "u", #{"n1" => "v1"}, #{"u" => #{"n" => "v"}}),
        {ok, {modified, #{"u" => #{"n1" => "v1"}}}}
    ),
    ?assertEqual(?M:put(
        "u", #{"n2" => "v2"}, #{"u" => #{"n" => "v", "n1" => "v1"}}),
        {ok, {modified, #{"u" => #{"n2" => "v2"}}}}
    ),
    ?assertEqual(?M:put(
        "u1", #{"n" => "v"}, #{"u" => #{"n" => "v", "n1" => "v1"}}),
        {ok, {created, #{"u" => #{"n" => "v", "n1" => "v1"},
                         "u1" => #{"n" => "v"}}}}
    ).

patch_test() ->
    ?assertEqual(?M:patch(
        "user_id", #{"metric_name" => "metric_value"}, #{}),
        {error, not_found}
    ),
    ?assertEqual(?M:patch(
            "user_id", #{"metric_name" => "new_metric_value"},
            #{"user_id" => #{"metric_name" => "metric_value"}}
        ),
        {ok, #{"user_id" => #{"metric_name" => "new_metric_value"}}}
    ),
    ?assertEqual(?M:patch(
        "u", #{"n1" => "v1"}, #{"u" => #{"n" => "v"}}),
        {ok, #{"u" => #{"n" => "v", "n1" => "v1"}}}
    ),
    ?assertEqual(?M:patch(
        "u", #{"n2" => "v2"}, #{"u" => #{"n" => "v", "n1" => "v1"}}),
        {ok, #{"u" => #{"n" => "v", "n1" => "v1", "n2" => "v2"}}}
    ),
    ?assertEqual(?M:patch(
        "u1", #{"n" => "v"}, #{"u" => #{"n" => "v", "n1" => "v1"}}),
        {error, not_found}
    ).

get_test() ->
    ?assertEqual(?M:get(
        "user_id", [], #{}),
        {error, not_found}
    ),
    ?assertEqual(?M:get(
        "user_id", [{filter, ["metric_name"]}], #{}),
        {error, not_found}
    ),
    ?assertEqual(?M:get(
        "u", [], #{"u" => #{"n1" => "v1", "n2" => "v2"}}),
        {ok, #{"n1" => "v1", "n2" => "v2"}}
    ),
    ?assertEqual(?M:get(
        "u", [{filter, ["n2"]}], #{"u" => #{"n1" => "v1", "n2" => "v2"}}),
        {ok, #{"n2" => "v2"}}
    ),
    ?assertEqual(?M:get(
        "u", [{filter, ["n3"]}], #{"u" => #{"n1" => "v1", "n2" => "v2"}}),
        {ok, #{}}
    ),
    ?assertEqual(?M:get(
        "u1", [], #{"u" => #{"n" => "v"}}),
        {error, not_found}
    ),
    ?assertEqual(?M:get(
        "u1", [{filter, ["n"]}], #{"u" => #{"n" => "v"}}),
        {error, not_found}
    ).

delete_test() -> 
    ?assertEqual(?M:delete(
        "user_id", #{}, #{}),
        {error, not_found}
    ),
    ?assertEqual(?M:delete(
        "user_id", ["metric_name"], #{}),
        {error, not_found}
    ),
    ?assertEqual(?M:delete(
        "u", ["n"], #{"u" => #{"n" => "v"}}),
        {ok, #{}}
    ),
    ?assertEqual(?M:delete(
        "u", ["n1"], #{"u" => #{"n1" => "v1", "n2" => "v2"}}),
        {ok, #{"u" => #{"n2" => "v2"}}}
    ),
    ?assertEqual(?M:delete(
        "u", ["n1", "n2"], #{"u" => #{"n1" => "v1", "n2" => "v2"}}),
        {ok, #{}}
    ),
    ?assertEqual(?M:delete(
        "u1", [], #{"u" => #{"n1" => "v1", "n2" => "v2"}}),
        {error, not_found}
    ),
    ?assertEqual(?M:delete(
        "u1", ["n1"], #{"u" => #{"n1" => "v1", "n2" => "v2"}}),
        {error, not_found}
    ),
    ?assertEqual(?M:delete(
        "u1", ["n1"], #{"u" => #{"n1" => "v1", "n2" => "v2"},
                        "u1" => #{"n1" => "v1", "n2" => "v2"}}),
        {ok, #{"u" => #{"n1" => "v1", "n2" => "v2"},
               "u1" => #{"n2" => "v2"}}}
    ).
