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

-record(state, {data, saved, file_path}).

start_link(Args) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Args, []).

init(Args) ->
    FilePath = proplists:get_value(file_path, Args),
    {ok, Data} = ?Backend:load(FilePath),
    ok = ?Backend:save(FilePath, Data),

    SaveInterval = proplists:get_value(save_interval, Args),
    {ok, _TRef} = timer:send_interval(SaveInterval, save),

    {ok, #state{data = Data, saved = true, file_path = FilePath}}.

handle_call({put, {UserId, Metrics}}, _From, State) ->
    case ?Backend:put(UserId, Metrics, data(State)) of
        {ok, {Impact, Data}} -> {reply, {ok, Impact}, set_data(Data, State)};
        {error, Reason} -> {reply, {error, Reason}, State}
    end;

handle_call({get, {UserId, Options}}, _From, State) ->
    {reply, ?Backend:get(UserId, Options, data(State)), State};

handle_call({patch, {UserId, Metrics}}, _From, State) ->
    case ?Backend:patch(UserId, Metrics, data(State)) of
        {ok, Data} -> {reply, ok, set_data(Data, State)};
        {error, Reason} -> {reply, {error, Reason}, State}
    end;

handle_call({delete, {UserId, MetricNames}}, _From, State) ->
    case ?Backend:delete(UserId, MetricNames, data(State)) of
        {ok, Data} -> {reply, ok, set_data(Data, State)};
        {error, Reason} -> {reply, {error, Reason}, State}
    end.

handle_cast(_Request, State) -> {norepy, State}.

handle_info(save, State = #state{saved = Saved, file_path = FilePath}) ->
    Saved orelse ?Backend:save(FilePath, data(State)), 
    {noreply, State#state{saved = true}}.

data(#state{data = DataState}) -> DataState.
set_data(DataState, State) ->
    State#state{data = DataState, saved = false}.
