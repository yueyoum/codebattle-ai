-module(ai).

-behaviour(gen_server).

%% API
-export([start_link/1,
         start_link/2,
         own_marine_ids/1,
         other_marine_ids/1,
         call_move/4,
         call_flares/2,
         call_gunshoot/4]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).


-define(TIMEOUT, 1000 * 10).

-record(vector2, {x=0, z=0}).
-record(marine, {
    id,
    hp,
    position=#vector2{},
    status,
    gunlasttime={{2013, 8, 9}, {0, 0, 0}},
    flares,
    role}).
-record(state, {sdk, mapx, mapz, started=false, own=dict:new(), others=dict:new(), flares_state}).

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
start_link(RoomId) ->
    gen_server:start_link(?MODULE, [RoomId, "red"], []).

start_link(RoomId, Color) ->
    gen_server:start_link(?MODULE, [RoomId, Color], []).

own_marine_ids(Pid) ->
    gen_server:call(Pid, own_marine_ids).

other_marine_ids(Pid) ->
    gen_server:call(Pid, other_marine_ids).

call_move(Pid, MarineId, X, Z) ->
    gen_server:call(Pid, {move, MarineId, X, Z}).

call_flares(Pid, MarineId) ->
    gen_server:call(Pid, {flares, MarineId}).

call_gunshoot(Pid, MarineId, X, Z) ->
    gen_server:call(Pid, {gunshoot, MarineId, X, Z}).

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
init([RoomId, Color]) ->
    <<A:32, B:32, C:32>> = crypto:strong_rand_bytes(12),
    random:seed({A, B, C}),
    {ok, SdkPid} = ai_sdk:start_link(self()),
    IP = {127, 0, 0, 1},
    Port = 8888,

    {ok, _Sock} = ai_sdk:connect(SdkPid, IP, Port),

    %% an ai must join a room before any action,
    %% so It's ok that we do this in init function.
    ok = ai_sdk:joinroom(SdkPid, RoomId, Color),
    {ok, #state{sdk=SdkPid}}.


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

% handle_call(_Request, _From, State) ->
%     {reply, ok, State}.

handle_call(own_marine_ids, _From, #state{own=Own} = State) ->
    {reply, dict:fetch_keys(Own), State};

handle_call(other_marine_ids, _From, #state{others=Others} = State) ->
    {reply, dict:fetch_keys(Others), State};


handle_call({move, MarineId, X, Z}, _From, #state{sdk=Sdk, own=Own} = State) ->
    Reply =
    case dict:is_key(MarineId, Own) of
        true ->
            ai_sdk:marineoperate(Sdk, MarineId, 'Run', X, Z);
        false ->
            not_found_this_marine
    end,
    {reply, Reply, State};

handle_call({flares, MarineId}, _From, #state{sdk=Sdk, own=Own} = State) ->
    Reply = 
    case dict:is_key(MarineId, Own) of
        true ->
            ai_sdk:marineoperate(Sdk, MarineId, 'Flares');
        false ->
            not_found_this_marine
    end,
    {reply, Reply, State};

handle_call({gunshoot, MarineId, X, Z}, _From, #state{sdk=Sdk, own=Own} = State) ->
    Reply = 
    case dict:is_key(MarineId, Own) of
        true ->
            ai_sdk:marineoperate(Sdk, MarineId, 'GunAttack', X, Z);
        false ->
            not_found_this_marine
    end,
    {reply, Reply, State}.


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
handle_cast({joinroomresponse, _RoomId, {vector2int, X, Z}, Marines}, State) ->
    Fun = fun(M, D) ->
        RM = make_new_marine_record(M),
        dict:store(RM#marine.id, RM, D)
    end,
    Own = lists:foldl(Fun, dict:new(), Marines),
    io:format("joinroomresponse, Marines = ~p~n", [Own]),
    {noreply, State#state{mapx=X, mapz=Z, own=Own}};


handle_cast({senceupdate, Own, Others}, State) ->
    NewState = action(Own, Others, State),
    Timeout = (random:uniform(3) + random:uniform(3)) * 1000,
    {noreply, NewState, Timeout};

handle_cast(startbattle, State) ->
    Timeout = (random:uniform(3) + random:uniform(3)) * 1000,
    {noreply, State#state{started=true}, Timeout};

handle_cast({endbattle, Reason, Win}, State) ->
    io:format("EndBattle, Reason = ~p, Win = ~p~n", [Reason, Win]),
    {stop, normal, State}.


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

handle_info(timeout, #state{sdk=Sdk, own=Own} = State) ->
    io:format("ai timeout, start Flares~n"),

    case choose_flares_id(Own) of
        undefined ->
            io:format("NO Flares Any More!!!~n");
        Id ->
            flares(Sdk, Id)
    end,
    {noreply, State}.


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
terminate(_Reason, _State) ->
    io:format("AI terminate, Reason = ~p~n", [_Reason]),
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


action(OwnMarines, OthersMarines, #state{sdk=Sdk, own=Own, others=Others} = State) ->
    %% Update Marine first
    UpdateFun = fun({marine, Id, _, _, _, _, _, _} = M, D) ->
        NewM =
        case dict:is_key(Id, D) of
            true ->
                update_marine_record(M, dict:fetch(Id, D));
            false ->
                make_new_marine_record(M)
        end,
        dict:store(Id, NewM, D)
    end,

    NewOwn = lists:foldl(UpdateFun, Own, OwnMarines),
    NewOthers = lists:foldl(UpdateFun, Others, OthersMarines),

    NewState = State#state{own=NewOwn, others=NewOthers},

    %% If got OthersMarines here, 
    %% means either you have Flares just now,
    %% or it should be others doing Flares, GunAttack,
    %% or others has been hitted,
    %% or others has hitted any other marines.
    io:format("Own ids = ~p~n", [dict:fetch_keys(NewOwn)]),

    case length(OthersMarines) of
        0 -> action_no_others(NewOwn, NewState);
        _ ->
            case is_flares(NewOwn) of
                true ->
                    action_after_own_flares(NewOwn, NewOthers, NewState);
                false ->
                    case is_flares(NewOthers) of
                        true ->
                            action_after_others_flares(NewOwn, NewOthers, NewState);
                        false ->
                            case is_gunattack(NewOthers) of
                                true ->
                                    action_after_others_shoot(NewOwn, NewOthers, NewState);
                                false ->
                                    case is_bullet_hitted(NewOthers) of
                                        true -> 
                                            action_after_bullet_hitted(NewOwn, NewOthers, NewState);
                                        false ->
                                            action_after_own_flares_2(NewOwn, NewOthers, NewState)
                                    end
                            end
                    end
            end
    end.



action_no_others(OwnMarines, State) ->
    io:format("No OthersMarines~n"),
    State.

action_after_own_flares(OwnMarines, OthersMarines, #state{sdk=Sdk} = State) ->
    %% Own Marines Flares, then I got all Marines in the battle.
    %% you can hold you fire and waiting the incoming message at next 1 senconds.
    %% so you can calculate others marines  run position via the two state.
    io:format("Got OthersMarines, due to I have Flares just now~n"),

    % all_move(OwnMarines, Sdk),
    all_attack(OwnMarines, OthersMarines, Sdk),
    State#state{flares_state=OthersMarines}.

action_after_others_flares(OwnMarines, OthersMarines, #state{sdk=Sdk} = State) ->
    %% Other Marines Flares, So He knew my state
    %% Don't worry about this, We can Idel here and waiting for he's GunAttack action.
    io:format("Got OthersMarines, Other Flares~n"),
    State.

action_after_own_flares_2(OwnMarines, OthersMarines, #state{sdk=Sdk, flares_state=FS} = State) ->
    %% need some calculate.
    io:format("Got Flares_2, Ignored...~n"),
    State.

action_after_others_shoot(OwnMarines, OthersMarines, #state{own=Own, sdk=Sdk} = State) ->
    %% Other Marines Shoot, He didn't know My state right now.
    io:format("Got OthersMarines, Other GunAttack~n"),
    AttackingMarines = all_attack(OwnMarines, OthersMarines, Sdk),
    UpdateGunShootTime = fun(#marine{id=Id}, D) ->
        M = dict:fetch(Id, D),
        NewM = M#marine{gunlasttime = calendar:now_to_datetime(now())},
        dict:store(Id, NewM, D)
    end,

    NewOwn = lists:foldl(UpdateGunShootTime, Own, AttackingMarines),
    State.

action_after_bullet_hitted(OwnMarines, OthersMarines, State) ->
    %% Buttle has hitted some marine.
    io:format("Got OthersMarines, Bullet Hitted~n"),
    State.


is_flares(Mdict) ->
    Fun = fun(#marine{status=Status}) -> Status =:= 'Flares' end,
    marines_test(Mdict, Fun).

is_gunattack(Mdict) ->
    Fun = fun(#marine{status=Status}) -> Status =:= 'GunAttack' end,
    marines_test(Mdict, Fun).

is_bullet_hitted(Mdict) ->
    Fun = fun(#marine{role=Role}) -> Role =:= 'Attacker' orelse Role =:= 'Injured' end,
    marines_test(Mdict, Fun).

marines_test(Mdict, Fun) ->
    L = [V || {_, V} <- dict:to_list(Mdict)],
    lists:any(Fun, L).




gun_attack(Sdk, Id, X, Z) ->
    ok = ai_sdk:marineoperate(Sdk, Id, 'GunAttack', X, Z).

move(Sdk, Id, X, Z) ->
    ok = ai_sdk:marineoperate(Sdk, Id, 'Run', X, Z).

flares(Sdk, Id) ->
    ok = ai_sdk:marineoperate(Sdk, Id, 'Flares').


gun_attack_and_run(Sdk, Id, Gx, Gz, Rx, Rz) ->
    gun_attack(Sdk, Id, Gx, Gz),
    move(Sdk, Id, Rx, Rz).


all_move(OwnMarines, Sdk) ->
    Fun = fun(_, #marine{id=Id}) ->
        move(Sdk, Id, random:uniform(50), random:uniform(50))
    end,

    dict:map(Fun, OwnMarines),
    ok.

all_attack(OwnMarines, OthersMarines, Sdk) ->
    case choose_gunattack_marine(OwnMarines) of
        [] ->
            io:format("No Marine Can Attack!!! Just Run!!!~n"),
            all_move(OwnMarines, Sdk),
            [];
        AttackingMarines ->
            Target = choose_marine_random(OthersMarines),
            Fun = fun(#marine{id=Id}) ->
                gun_attack_and_run(Sdk,
                                   Id,
                                   Target#marine.position#vector2.x,
                                   Target#marine.position#vector2.z,
                                   random:uniform(50),
                                   random:uniform(50))
            end,
            lists:foreach(Fun, AttackingMarines),
            AttackingMarines
    end.



update_marine_record({marine, Id, Hp, {vector2, X, Z}, Status, _, FlaresAmount, Role}, M) ->
    M#marine{hp=Hp, position=#vector2{x=X, z=Z}, status=Status, flares=FlaresAmount, role=Role}.

make_new_marine_record({marine, Id, Hp, {vector2, X, Z}, Status, _, FlaresAmount, Role}) ->
    #marine{id=Id, hp=Hp, position=#vector2{x=X, z=Z}, status=Status, flares=FlaresAmount, role=Role}.


choose_flares_id(OwnMarines) ->
    %% when there is only one marine has flares, return It's id
    %% or two or more have flares, choose one which has more hp
    %% or no one has flares, return undefined

    HasFlares = lists:filter(fun(#marine{flares=FlaresAmount}=M) -> FlaresAmount > 0 end,
        [V || {_, V} <- dict:to_list(OwnMarines)]
        ),

    case length(HasFlares) of
        0 -> undefined;
        1 ->
            Target = lists:nth(1, HasFlares),
            Target#marine.id;
        _ ->
            Target = lists:nth(1, lists:sort(fun(A, B) -> A#marine.hp > B#marine.hp end, HasFlares)),
            Target#marine.id
    end.


choose_gunattack_marine(OwnMarines) ->
    lists:filter(
        fun(#marine{gunlasttime=T}) -> can_make_gun_shoot(T) end,
        [V || {_, V} <- dict:to_list(OwnMarines)]
        ).


can_make_gun_shoot(GunLastTime) ->
    {A, {H, M, S}} = calendar:time_difference(
        GunLastTime,
        calendar:now_to_datetime(now())
        ),
    case A of
        Day when Day > 0 -> true;
        Day when Day =:=0 -> H > 0 orelse M > 0 orelse S >= 2;
        _ -> false
    end.


choose_marine_random(Ms) ->
    L = dict:to_list(Ms),
    {_, M} = lists:nth(random:uniform(length(L)), L),
    M.

choose_marine_by_hp(Ms) ->
    L = [V || {_, V} <- dict:to_list(Ms)],
    Fun = fun(M1, M2) -> M1#marine.hp =< M2#marine.hp end,
    HpMs = lists:sort(Fun, Ms),
    lists:nth(1, HpMs).
