% erl -name apa -setcookie apa
% epmd -names

-module(opponent).
-compile(export_all).
-behaviour(gen_server).

-record(state, {name, health}).
    
% Client side 
start(Name, Health) ->
    {ok, _Pid} = gen_server:start_link({global,Name}, ?MODULE, {Name, Health}, []),
    Name.

attack(MyName, OpponentName, Damage) ->
    say("~p attack ~p~n",[global:whereis_name(MyName), global:whereis_name(OpponentName)]),
    gen_server:call({global, MyName}, {attack, OpponentName, Damage}).

handle_attack(ReceiverName, Damage) ->
    say("~p received ~p damage~n", [global:whereis_name(ReceiverName), Damage]),
    gen_server:cast({global, ReceiverName}, {handle_attack, Damage}).

report_health(Name) ->
    gen_server:call({global, Name}, report_health).

 % - - - - -- -- - - - - --- - - --- - - -

% Server side

init({Name, Health}) ->
    say("init({~p,~p})~n",[Name,Health]),
    % Register user to world_server
    say("Before world_server:register~n"),
    ok = gen_server:call({global, world_server}, {register_opponent, Name}),
    say("After world_server:register~n"),
    {ok, #state{name = Name,
		health = Health}}.

terminate(Reason, State) ->
    say("Just died with reason ~p, state = ~p~n", 
	[Reason, State]).

handle_cast({handle_attack, Damage}, 
	    State = #state{health = PreviousHealth}) ->
    say("Received an attack, health status before attack: ~p~n",
	[State#state.health]),	
    NewState = State#state{health = PreviousHealth - Damage},
    
    manage(Damage * 100),
    
    say("Health after shoot : ~p~n",
	[NewState#state.health]),
    if NewState#state.health > 0 ->
	    {noreply, NewState};
       true ->
	    {stop, normal, NewState}
    end.

handle_call({attack, OpponentName, Damage}, _From, State) ->
    case global:whereis_name(OpponentName) of
	undefined ->
	    say("attack to unregistered opponent ~p~nRegistered opponents: ~p",
		[global:whereis_name(OpponentName), world_server:get_registered_opponents()]),
	    {reply, opponent_not_registered, State};
	_Pid ->
	    say("Attacking ~p~n",[OpponentName]),
	    manage(10),
	    opponent:handle_attack(OpponentName, Damage),
	    {reply, ok, State}
    end;

handle_call(report_health, _From, State)->
    say("~p healthstatus: ~p~n",[self(), State#state.health]),
    {reply, State#state.health, State};

handle_call(terminate, _From, State)->
    say("~p terminate: ~p~n",[self(), State]),
    {reply, State, State};

handle_call(Arg, _From, State)->
    say("~p received an untrapped call(~p): ~p~n",[self(), Arg, State]),
    {reply, State, State}.

handle_info(Info, State) ->
    say("~p received info ~p, ~p.", [self(), Info, State]),
    {noreply, State}.

code_change(OldVsn, State, Extra) ->
    say("code_change ~p, ~p, ~p", [OldVsn, State, Extra]),
    {ok, State}.    

% local functions

say(Format) ->
  say(Format, []).
say(Format, Data) ->
  io:format("~p:~p: ~s~n", [?MODULE, self(), io_lib:format(Format, Data)]).


%%-----------------------------------------------------------------------------
%% manage(Work) will consume CPU to simulate work
%%----------------------------------------------------------------------------
manage(Work) ->
    tail_heal_h(Work,0,0,0).

%% tail recursion
tail_heal_h(End,N,Lastheal,SecondLastheal) ->
  case N of
    End -> Lastheal + SecondLastheal;
    0 -> tail_heal_h(End, 1, 0, 0) ;
    1 -> tail_heal_h(End, 2, 1, 0) ;
    _ -> tail_heal_h(End,N+1,SecondLastheal+Lastheal,Lastheal)
  end.

%---------------------------
show_progress_bar(Sleep) ->
    Resolution = 10,
    show_progress_bar(round(Sleep/Resolution),Resolution).

show_progress_bar(_Sleep, 0) ->
    io:format("~n");
show_progress_bar(Sleep, Count) ->
    timer:sleep(Sleep),
    io:format("."),
    show_progress_bar(Sleep,Count-1).
