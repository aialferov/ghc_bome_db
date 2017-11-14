-module(ghc_bome_db_app).
-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    {ok, Env} = application:get_key(env),
    ghc_bome_db_sup:start_link(Env).

stop(_State) -> ok.
