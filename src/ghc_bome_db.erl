-module(ghc_bome_db).
-compile({no_auto_import,[get/1]}).

-export([
    start/0, stop/0,
    put/2, patch/2, get/2, delete/2
]).

-define(Server, ghc_bome_db_server).

start() -> application:ensure_all_started(?MODULE).
stop() -> application:stop(?MODULE).

put(Id, Data) -> call({put, {Id, Data}}).
patch(Id, Data) -> call({patch, {Id, Data}}).
get(Id, Options) -> call({get, {Id, Options}}).
delete(Id, DataKeys) -> call({delete, {Id, DataKeys}}).

call(Request) -> gen_server:call(?Server, Request).
