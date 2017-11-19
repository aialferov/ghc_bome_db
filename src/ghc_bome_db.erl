-module(ghc_bome_db).
-compile({no_auto_import,[get/1]}).

-export([
    start/0, stop/0,
    put/2, patch/2, get/2, delete/2
]).

-define(Server, ghc_bome_db_server).

start() -> application:ensure_all_started(?MODULE).
stop() -> application:stop(?MODULE).

put(UserId, Metrics) -> call({put, {UserId, Metrics}}).
patch(UserId, Metrics) -> call({patch, {UserId, Metrics}}).
get(UserId, Options) -> call({get, {UserId, Options}}).
delete(UserId, MetricNames) -> call({delete, {UserId, MetricNames}}).

call(Request) -> gen_server:call(?Server, Request).
