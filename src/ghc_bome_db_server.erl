-module(ghc_bome_db_server).
-behaviour(gen_server).

-export([
    start_link/1,

    init/1,

    handle_call/3,
    handle_cast/2,
    handle_info/2
]).

-define(Backend, ghc_bome_db_backend).

-define(FileName, "/tmp/ghc_bome.db").

-define(SaveInterval, 1000). % milliseconds

start_link(Args) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Args, []).

init(_Args) ->
    {ok, _TRef} = timer:send_interval(?SaveInterval, save),
    {ok, {saved, ?Backend:load(?FileName)}}.

handle_call({put, {User, {Type, Value}}}, _From, {_Saved, Data}) ->
    {reply, ok, {unsaved, ?Backend:put(User, Type, Value, Data)}};

handle_call({get, {User, Type}}, _From, {Saved, Data}) ->
    {reply, ?Backend:get(User, Type, Data), {Saved, Data}};

handle_call({get, User}, _From, {Saved, Data}) ->
    {reply, ?Backend:get(User, Data), {Saved, Data}};

handle_call({delete, {User, Type}}, _From, {_Saved, Data}) ->
    {reply, ok, {unsaved, ?Backend:delete(User, Type, Data)}};

handle_call({delete, User}, _From, {_Saved, Data}) ->
    {reply, ok, {unsaved, ?Backend:delete(User, Data)}}.

handle_cast(_Request, Data) -> {norepy, Data}.

handle_info(save, {Saved, Data}) ->
    Saved == saved orelse ?Backend:save(?FileName, Data),
    {noreply, {saved, Data}}.
