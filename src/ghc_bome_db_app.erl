-module(ghc_bome_db_app).
-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, StartArgs) -> ghc_bome_db_sup:start_link(StartArgs).
stop(_State) -> ok.
