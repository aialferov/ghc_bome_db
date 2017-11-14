-module(ghc_bome_db).
-compile({no_auto_import,[get/1]}).

-export([
    start/0, stop/0,

    put/2,
    get/2, get/1,
    delete/2, delete/1
]).

-define(Server, ghc_bome_db_server).

start() -> application:ensure_all_started(?MODULE).
stop() -> application:stop(?MODULE).

put(User, {Type, Value}) -> call({put, {User, {Type, Value}}}).

get(User, undefined) -> get(User);
get(User, Type) -> call({get, {User, Type}}).
get(User) -> call({get, User}).

delete(User, undefined) -> delete(User);
delete(User, Type) -> call({delete, {User, Type}}).
delete(User) -> call({delete, User}).

call(Request) -> gen_server:call(?Server, Request).
