-module(world_server).
-compile(export_all).
-behaviour(gen_server).

-define(register_opponents, register_opponents).
-record(state, {status = ?register_opponents, opponents = []}).

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

register_opponent(Name) ->
    gen_server:call({global,?MODULE}, {register_opponent, Name}).

get_active_opponents() ->
    gen_server:call({global,?MODULE}, get_active_opponents).

% Server side
init(_Arg) ->
    say("The Game of Code is ready to register opponents~n"),
    {ok, #state{status = opponents}}.

handle_call(start_battle, _From, State) ->
    Opponents = State#state.opponents,
    say("The following opponents are registered: ~p~nLet the battle begin!!!~n",[Opponents]),
    {reply, Opponents, State#state{status = started}};

handle_call({register_opponent, Name}, {FromPid,_FromTag}, State) ->
    Opponents = State#state.opponents,
    say("Before add: ~p~n",[Opponents]),

    %% Validate that Name is not already registered and that it's the 
    %% caller pid is associated with the registration name
    case {lists:member(Name, Opponents), global:whereis_name(Name)} of
	{false, FromPid} ->
	    NewOpponents = [Name | Opponents],
	    say("After add: ~p~n",[NewOpponents]),
	    {reply, ok, State#state{opponents = NewOpponents}};
	{true, _} ->
	    {reply, {error, name_already_registered}, State};
	{_, Pid} ->
	    say("~p is not associated with the name ~p, but instead ~p~n",[FromPid,Name,Pid]),
	    {reply, {error, name_not_associated_with_caller_pid}}
    end;
    
handle_call(get_active_opponents, _From, #state{opponents = Opponents} = State)->
    ActiveOpponents = [Opponent||Opponent <- Opponents, 
				 global:whereis_name(Opponent) =/= undefined],
    {reply, ActiveOpponents, State#state{opponents = ActiveOpponents}};

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

