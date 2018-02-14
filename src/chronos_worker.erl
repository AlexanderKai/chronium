-module(chronos_worker).
-behaviour(gen_server).

-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3 ]).

-record(state, {last_execute, retries, max_retries}).

start_link(Args) ->
    gen_server:start_link(?MODULE, Args, []).

	
init(_Args) ->
	process_flag(trap_exit, true),
	{ok, #state{retries=0, last_execute=undefined, max_retries=undefined}}.

find(Str, Symb) ->
	find(Str, Symb, 1).

find([], _, _) ->
	not_found;

find([H|T], E, Pos) ->
	[E1|_] = E,
	case H == E1 of
   		true -> Pos;
	 	_ -> find(T, E, Pos+1)
	end.	 

compare(V1, V2) ->
	AllPos = find(V1, "*"),
	IntervalPos = find(V1, "-"),
	SlashPos = find(V1, "/"),

	case [AllPos, IntervalPos, SlashPos] of
		[not_found, not_found, not_found] -> 
			l:l2i(V1) == V2;
		[_, not_found, not_found] -> 
			true;
		[not_found, _, not_found] ->
			L = l:l2i(lists:sublist(V1,1, IntervalPos-1)),
			H = l:l2i(lists:sublist(V1, IntervalPos+1, 10)),
			L =< V2 andalso V2=< H;
		[not_found, _, _] ->
			L = l:l2i(lists:sublist(V1,1, IntervalPos-1)),
			H = l:l2i(lists:sublist(V1, IntervalPos+1, SlashPos - IntervalPos - 1)),
			Divider = l:l2i(lists:sublist(V1, SlashPos+1, 10)),
			L =< V2 andalso V2=< H andalso (V2 rem Divider == 0);
		[_, not_found, _] ->
			Divider = l:l2i(lists:sublist(V1, SlashPos+1, 10)),
			V2 rem Divider == 0;
		_ -> error
	end.

compare_list(L1, L2) ->
	compare_list(L1, L2, [second, minute, hour, day, month, dayoftheweek, year]).

compare_list([],[], _) ->
	true;

compare_list([H1|T1], [H2|T2], Granularity) ->
	case compare(H1, H2) of
		true -> compare_list(T1,T2, Granularity);
		false -> false;
		V -> V
	end.

time_is_come(Time, Crontab) ->	
	{{YY,MM,DD},{H,M,S}} = Time,
	{Second, Minute, Hour, Day, Month, DayOfWeek, Year} = Crontab,
	L1 = [Second, Minute, Hour, Day, Month, DayOfWeek, Year],
	L2 = [S, M, H, DD, MM, calendar:day_of_the_week({YY, MM, DD}), YY],
	compare_list(L1, L2).

run(Key, Last, Tab) ->
	[{Name, Settings}] = ets:lookup(Tab, Key),
	SettingsWithLast = l:pl_sk(last, Last, Settings),
	ets:insert(Tab, {Name, l:pl_sk(state, running, SettingsWithLast)}).
    
stop(Key, Tab) ->
	[{Name, Settings}] = ets:lookup(Tab, Key),
	ets:insert(Tab, {Name, l:pl_sk(state, idle, Settings)}).


handle_call({update}, _From, #state{}=State) ->
	{reply, [], State}.	

handle_cast({check, _From, Args, Tab, Worker}, State) ->
	{Name, Settings, CommonSettings} = Args,
	try

    	%MaxTries = proplists:get_value(max_retries, Settings),
		%PeriodRestart = proplists:get_value(period_restart, Settings),
	    Crontab = proplists:get_value(time, Settings),
	    MFA = proplists:get_value(mfa, Settings),
		Last = proplists:get_value(last, Settings),
		{M, F, A} = MFA,

		DebugPrint = proplists:get_value(send_email_on_crash, CommonSettings),

		Now = l:current_time(),
		case [time_is_come(Now, Crontab), Last =/= Now] of
			[true, true] ->
				case DebugPrint of
					on ->
						io:format("Fired ~p~nTime ~p~n", [Name, Now]);
					_ -> []
				end,
	            run(Name, Now, Tab),
				erlang:apply(M, F, A),
	            stop(Name, Tab),
				poolboy:checkin(chronos_worker, Worker),
				Now;
			_ ->
				poolboy:checkin(chronos_worker, Worker),
				Last
		end,
		{noreply, State}
	catch
		E1:E2 -> 
			io:format("Job is crashed!~n~p ~p~n", [E1, E2]),
			io:format("~p~n", [erlang:get_stacktrace()]),
			stop(Name, Tab),
			poolboy:checkin(chronos_worker, Worker),
			{noreply, State}
	end;
	

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({check, From, Name, MFA}, #state{retries=Tries}=State) ->
	{M, F, A} = MFA,
	try
		erlang:apply(M, F, A)
	catch
		E1:E2 -> []
	end,
	From ! {is_stopped, Name}, 
    {noreply, State};

handle_info(A, State) ->
	io:format("~nChronos. Unknown message ~n~p~n", [A]),
	{noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
