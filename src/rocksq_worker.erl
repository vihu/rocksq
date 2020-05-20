%%--------------------------------------------------------------------
%% Expose a simple rocksdb backed queue interface
%% --------------------------------------------------------------------
-module(rocksq_worker).

-behaviour(gen_server).

%%--------------------------------------------------------------------
%% API exports
%% --------------------------------------------------------------------
-export([start_link/1,
         is_empty/0,
         enqueue/1,
         deque/0
        ]).

%%--------------------------------------------------------------------
%% gen_server exports
%% --------------------------------------------------------------------
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

%% TODO: support multiple queues
-define(QID, 0).

-record(state, {
          db :: rocksdb:db_handle(),
          %% track seqeuence id of the default queue
          first_id = undefined :: undefined | non_neg_integer(),
          last_id = undefined :: undefined | non_neg_integer()
         }).


-type state() :: #state{}.

%%--------------------------------------------------------------------
%% API
%% --------------------------------------------------------------------
start_link(Args) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Args, []).

%%--------------------------------------------------------------------
%% @doc Insert item at the end of the queue
%% @end
%% --------------------------------------------------------------------
-spec enqueue(Item :: binary()) -> ok.
enqueue(Item) ->
    gen_server:cast(?MODULE, {enqueue, Item}).

%%--------------------------------------------------------------------
%% @doc Check if the queue is empty
%% @end
%% --------------------------------------------------------------------
-spec is_empty() -> boolean().
is_empty() ->
    gen_server:call(?MODULE, is_empty).

%%--------------------------------------------------------------------
%% @doc Remove and return item from the beginning of the queue
%% @end
%% --------------------------------------------------------------------
-spec deque() -> {error, any()} | {ok, binary()}.
deque() ->
    gen_server:call(?MODULE, deque).

%%--------------------------------------------------------------------
%% gen_server callbacks
%% --------------------------------------------------------------------
init(Args) ->
    BaseDir = proplists:get_value(base_dir, Args),
    DBPath = filename:join([BaseDir, "queue.db"]),
    ok = filelib:ensure_dir(DBPath),
    Opts = [{create_if_missing, true}],
    {ok, DB} = rocksdb:open(DBPath, Opts),
    {ok, #state{db=DB}}.

handle_call(is_empty, _, #state{first_id=undefined, last_id=undefined}=State) ->
    {reply, true, State};
handle_call(is_empty, _, State) ->
    {reply, false, State};
handle_call(deque, _, #state{first_id=undefined, last_id=undefined}=State) ->
    {reply, {error, empty_queue}, State};
handle_call(deque, _, State) ->
    {Reply, NewState} = deque(State),
    {reply, Reply, NewState};
handle_call(_, _, State) ->
    {reply, ok, State}.

handle_cast({enqueue, Item}, #state{db=DB, first_id=undefined, last_id=undefined}=State) ->
    ok = rocksdb:put(DB, <<?QID, 0>>, Item, [{sync, true}]),
    {noreply, State#state{first_id=0, last_id=0}};
handle_cast({enqueue, Item}, #state{db=DB, last_id=Last}=State) ->
    NewLast = Last + 1,
    ok = rocksdb:put(DB, <<?QID, NewLast>>, Item, [{sync, true}]),
    {noreply, State#state{last_id=NewLast}};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%% Internal functions
%% --------------------------------------------------------------------

-spec deque(State :: state()) -> {{ok, binary()}, state()} | {{error, any()}, state()}.
deque(#state{db=DB, first_id=First, last_id=Last}=State) ->
    case rocksdb:get(DB, <<?QID, First>>, []) of
        {ok, _}=Res ->
            %% We found the item in the queue,
            %% Update first id, last remains same
            NewFirst = First + 1,
            case NewFirst >= Last + 1 of
                true ->
                    %% The queue has been emptied here
                    {Res, State#state{first_id=undefined, last_id=undefined}};
                false ->
                    %% Keep dequeing
                    {Res, State#state{first_id=NewFirst}}
            end;
        not_found ->
            {{error, not_found}, State};
        {error, _}=E ->
            {E, State}
    end.
