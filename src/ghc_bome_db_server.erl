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

-record(state, {data_state, saved, file_path}).

start_link(Args) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Args, []).

init(Args) ->
    SaveInterval = proplists:get_value(save_interval, Args),
    {ok, _TRef} = timer:send_interval(SaveInterval, save),

    FilePath = proplists:get_value(file_path, Args),
    {ok, DataState} = ?Backend:load(FilePath),
    ok = ?Backend:save(FilePath, DataState),

    {ok, #state{data_state = DataState, saved = true, file_path = FilePath}}.

handle_call({put, {Id, Data}}, _From, State) ->
    case ?Backend:put(Id, Data, data_state(State)) of
        {ok, {Impact, NewDataState}} ->
            {reply, {ok, Impact}, set_data_state(NewDataState, State)};
        {error, Reason} ->
            {reply, {error, Reason}, State}
    end;

handle_call({get, {Id, Options}}, _From, State) ->
    {reply, ?Backend:get(Id, Options, data_state(State)), State};

handle_call({Action, {Id, Data}}, _From, State) ->
    case ?Backend:Action(Id, Data, data_state(State)) of
        {ok, NewDataState} -> {reply, ok, set_data_state(NewDataState, State)};
        {error, Reason} -> {reply, {error, Reason}, State}
    end.

handle_cast(_Request, State) -> {norepy, State}.

handle_info(save, State = #state{saved = Saved, file_path = FilePath}) ->
    Saved orelse ?Backend:save(FilePath, data_state(State)), 
    {noreply, State#state{saved = true}}.

data_state(#state{data_state = DataState}) -> DataState.
set_data_state(DataState, State) ->
    State#state{data_state = DataState, saved = false}.
