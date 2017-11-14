-module(ghc_bome_db_sup).
-behaviour(supervisor).

-export([start_link/1, init/1]).

start_link(Args) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, Args).

init(Args) -> {ok, {#{}, [
    #{id => ghc_bome_db,
      start => {ghc_bome_db_server, start_link, [Args]}}
]}}.
