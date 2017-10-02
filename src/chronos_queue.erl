-module(chronos_queue).
-behaviour(gen_server).
-include_lib("main.hrl").

-export([start_link/1]).
-export([init/1, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).

-record(state, {timer, tab}).

start_link(Args) ->
    gen_server:start_link(?MODULE, Args, []).

	
init(_Args) ->

	process_flag(trap_exit, true),
	
	{ok, Settings} = application:get_env(chronos, settings),
	
	Tab = ets:new(chronos, [public, named_table, set]),
    put(tab, Tab),

	%CommonSettings = proplists:get_value(settings, Settings),
    put(common_settings, Settings),

	{ok, Jobs} = application:get_env(chronos, jobs),
	io:format("Chronos Jobs~n~p~nSettings~n~p~n", [Jobs, Settings]),
	ets:insert(chronos, initialize_settings(Jobs)),
	Timer = erlang:send_after(1, self(), poll),
	
	{ok, #state{timer=Timer, tab = Tab}}.

initialize_settings(Jobs) ->
	[
		{Name, lists:flatten([{state, idle}, {last, undefined}|Settings])}
	||
	{Name, Settings} <- Jobs].

do_jobs() ->
	First = ets:first(chronos),
	case First of
		'$end_of_table' -> [];
		_ -> do_jobs(First, [])
	end.

do_jobs(Key, Pool) ->
	[Row] = ets:lookup(chronos, Key),
	{Name, Settings} = Row,
	CommonSettings = get(common_settings),

	W = case proplists:get_value(state, Settings) of
		idle -> 
			try
				Worker = poolboy:checkout(chronos_worker),
				gen_server:cast(Worker, {check, self(), {Name, Settings, CommonSettings}, get(tab), Worker}),
				Worker
			catch
				E1:E2 ->
				?dbgv("", E1), 
				?dbgv("", E2),
				?dbgv("stack", erlang:get_stacktrace())
			end;
		_ -> []
	end,

	Next = ets:next(chronos, Key),
	case Next of
		'$end_of_table' -> [];
		_ -> do_jobs(Next, [W|Pool])
	end.

handle_cast(_Msg, State) ->
    {reply, State}.

handle_info(poll, #state{timer=OldTimer}=State) ->
	erlang:cancel_timer(OldTimer),
	do_jobs(),
	Timer = erlang:send_after(500, self(), poll),
    {noreply, #state{timer=Timer}}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
