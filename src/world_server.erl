-module(world_server).
-compile(export_all).
-behaviour(gen_server).

-record(state, {status = register_opponents, opponents = []}).

% Client side 
start(_Type, _Args) ->
    gen_server:start({global,?MODULE}, ?MODULE, [], []).

stop() ->
    gen_server:call({global, ?MODULE}, stop).

start_battle() ->
    say("The Game of Code battle has started!!!~n"),
    gen_server:call({global,?MODULE}, start_battle).

stop_battle() ->
    ok.

register_opponent(Pid) ->
    gen_server:call({global,?MODULE}, {register_opponent, Pid}).

get_active_opponents() ->
    gen_server:call({global,?MODULE}, get_active_opponents).

attack(0) ->
    ok;
attack(Tick) ->
    gen_server:call({global,?MODULE}, attack),
    say("Let's see who survives this attack~n"),
    show_progress_bar(1000),
    RemainingOpponents = gen_server:call({global,?MODULE}, get_active_opponents),
    say("Remaining opponents: ~p~n",[RemainingOpponents]),
    attack(Tick-1).

 % - - - - -- -- - - - - --- - - --- - - -

% Server side
init(_Arg) ->
    say("The Game of Code is ready to register opponents~n"),
    %process_flag(trap_exit, true),
    {ok, #state{status = opponents}}.

handle_call(start_battle, _From, State) ->
    Opponents = State#state.opponents,
    say("The following opponents are registered: ~p~nLet the battle begin!!!~n",[Opponents]),
    {reply, Opponents, State#state{status = started}};

handle_call({register_opponent, Pid}, _From, State) ->% when State#state.status == registered_opponents ->
    Opponents = State#state.opponents,
    say("Registered opponents: ~p~n",[Opponents]),
    NewOpponents = [Pid| Opponents],
    say("After add: ~p~n",[NewOpponents]),
     {reply, NewOpponents, State#state{opponents = NewOpponents}};

handle_call(get_active_opponents, _From, #state{opponents = Opponents} = State)->
%    Opponents = State#state.opponents,
    ActiveOpponents = [Opponent||Opponent <- Opponents, 
				 is_process_alive(Opponent)],
    {reply, ActiveOpponents, State#state{opponents = ActiveOpponents}};

handle_call(attack, _From, State) ->
    Opponents = State#state.opponents,
    Attack = fun(A,B) -> 
                    opponent:attack(A, B, 200),
		    say("~p is attacking ~p~n",[A, B])
     end,
    [Attack(A,B)|| A <- Opponents, B <- Opponents, A /= B],

    {reply, attack_ended, State};

handle_call(stop, _From, State)->
    say("Thanks for playing, we are closing this session~n"),
    {stop, game_ended, State}.

handle_cast(Args, State) ->
    say("handle_cast(~p) ~p~n",[Args, State]),
    {noreply, State}.

handle_info({'EXIT', KilledPid, normal}, State) ->
    say("Server info: KilledPid =~p, State = ~p~n",[KilledPid, State]),
    NewOpponents = lists:delete(KilledPid, State#state.opponents),
    say("NewOpponents: ~p~n",[NewOpponents]),
    {no_reply, State#state{opponents = NewOpponents}};

handle_info(Info, State) ->
    say("handle_info(~p, ~p)", [Info, State]),
    {noreply, State}.

terminate(Reason, State = #state{opponents = Winner}) ->
    say("The Game of Code has ended with reason ~p and state ~p and we have a winner: ~p~n", [Reason, State,Winner]),
    ok.
    
code_change(_OldVsn, State, _Extra) ->
    say("code_change ~p, ~p, ~p", [_OldVsn, State, _Extra]),
    {ok, State}.    

% local functions
show_progress_bar(Sleep) ->
    Resolution = 10,
    show_progress_bar(round(Sleep/Resolution),Resolution).

show_progress_bar(_Sleep, 0) ->
    io:format("~n");
show_progress_bar(Sleep, Count) ->
    timer:sleep(Sleep),
    io:format("."),
    show_progress_bar(Sleep,Count-1).

say(Format) ->
  say(Format, []).
say(Format, Data) ->
  io:format("~p:~p: ~s~n", [?MODULE, self(), io_lib:format(Format, Data)]).

