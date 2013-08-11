-module(ai_sdk).

-behaviour(gen_server).

%% API
-export([start_link/1,
         connect/3,
         joinroom/3,
         marineoperate/3,
         marineoperate/5]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).


-record(state, {worker, roomid, mapx, mapz, sock}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link(WorkerPid) ->
    gen_server:start_link(?MODULE, [WorkerPid], []).

connect(Pid, IP, Port) ->
    gen_server:call(Pid, {connect, IP, Port}).

joinroom(Pid, RoomId, Color) ->
    gen_server:call(Pid, {joinroom, RoomId, Color}).

marineoperate(Pid, MarineId, Status) ->
    gen_server:call(Pid, {marineoperate, MarineId, Status}).

marineoperate(Pid, MarineId, Status, Tx, Tz) ->
    gen_server:call(Pid, {marineoperate, MarineId, Status, Tx, Tz}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([WorkerPid]) ->
    {ok, #state{worker=WorkerPid}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call({connect, IP, Port}, _From, State) ->
    {ok, Sock} = gen_tcp:connect(IP, Port,
            [binary, inet, {reuseaddr, true}, {active, once}, {nodelay, true}, {packet, 4}]),

    {reply, {ok, Sock}, State#state{sock=Sock}};

handle_call({joinroom, RoomId, Color}, _From, #state{sock=Sock} = State) ->
    JoinroomRequest = api_pb:encode_cmd({cmd, joinroom,
        {joinroom, RoomId, Color},
        undefined,
        undefined
        }),
    ok = gen_tcp:send(Sock, JoinroomRequest),
    {reply, ok, State};


handle_call({marineoperate, MarineId, Status}, _From, #state{sock=Sock} = State) ->
    Cmd = api_pb:encode_cmd({cmd, marineoperate,
        undefined,
        undefined,
        {marineoperate, MarineId, Status, undefined}
        }),
    ok = gen_tcp:send(Sock, Cmd),
    {reply, ok, State};

handle_call({marineoperate, MarineId, Status, Tx, Tz}, _From, #state{sock=Sock} = State) ->
    Cmd = api_pb:encode_cmd({cmd, marineoperate,
        undefined,
        undefined,
        {marineoperate, MarineId, Status, {vector2, Tx, Tz}}
        }),
    ok = gen_tcp:send(Sock, Cmd),
    {reply, ok, State}.



%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(_Request, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------

handle_info({tcp, Sock, Data}, State) ->
    Reply =
    case parse_data(Data, State) of
        {ok, NewState} ->
            inet:setopts(Sock, [{active, once}]),
            {noreply, NewState};
        error ->
            {stop, error_code, State}
    end,
    Reply;


handle_info({tcp_closed, _}, State) ->
    io:format("tcp closed~n"),
    {stop, normal, State};

handle_info({tcp_error, _, Reason}, State) ->
    io:format("tcp error, reason: ~p~n", [Reason]),
    {stop, Reason, State};

handle_info(timeout, State) ->
    io:format("timeout..."),
    {stop, normal, State}.



%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, State) ->
    gen_tcp:close(State#state.sock),
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================


parse_data(Data, State) ->
    {message, MsgType, CmdResponse, SenceUpdate, EndBattle} = api_pb:decode_message(Data),
    case MsgType of
        cmdresponse -> cmdresponse(CmdResponse, State);
        senceupdate -> senceupdate(SenceUpdate, State);
        startbattle -> startbattle(State);
        endbattle -> endbattle(EndBattle, State)
    end.


cmdresponse({cmdresponse, 0, joinroom, JoinRoomResponse, _}, #state{worker=Worker} = State) ->
    {joinroomresponse, RoomId, {vector2int, X, Z}, _} = JoinRoomResponse,
    gen_server:cast(Worker, JoinRoomResponse),
    {ok, State#state{roomid=RoomId, mapx=X, mapz=Z}};

cmdresponse({cmdresponse, 0, marineoperate, _, _}, State) ->
    {ok, State};

cmdresponse({cmdresponse, Ret, Cmd, _, _}, State) ->
    io:format("Cmd ~p Error, Error Code = ~p~n", [Cmd, Ret]),
    {ok, State}.

senceupdate({senceupdate, Own, Others} = Data, #state{worker=Worker} = State) ->
    % io:format("SenceUpdate, at ~p~n", [calendar:now_to_datetime(now())]),
    % io:format("Own = ~p~n", [Own]),
    % io:format("Others = ~p~n", [Others]),
    gen_server:cast(Worker, Data),
    {ok, State}.

startbattle(#state{worker=Worker} = State) ->
    io:format("startbattle!~n"),
    gen_server:cast(Worker, startbattle),
    {ok, State}.

endbattle(EndBattle, #state{worker=Worker} = State) ->
    io:format("endbattle~n"),
    gen_server:cast(Worker, EndBattle),
    {ok, State}.