-module(ghc_bome_db).

-export([
    start/0, stop/0,

    put/3,
    get/2, get/1,
    delete/2, delete/1
]).

-define(Server, ghc_bome_db_server).

start() -> application:ensure_all_started(?MODULE).
stop() -> application:stop(?MODULE).

put(User, Type, Value) ->
    gen_server:call(?Server, {put, {User, {Type, Value}}}).

get(User, Type) -> gen_server:call(?Server, {get, {User, Type}}).
get(User) -> gen_server:call(?Server, {get, User}).

delete(User, Type) -> gen_server:call(?Server, {delete, {User, Type}}).
delete(User) -> gen_server:call(?Server, {delete, User}).
