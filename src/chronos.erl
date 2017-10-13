-module(chronos).
-behaviour(supervisor).
-behaviour(application).
-export([init/1, start/2, stop/1, start_link/0]).


start(_,_) -> chronos:start_link().
start_link() -> supervisor:start_link({local,?MODULE},?MODULE,[]).
stop(_)    -> ok.


init([]) -> 
	try
	{ok,Pools} = application:get_env(chronos, pools),
	[Pools_1, Pools_2] = Pools,
	Pools1 = [Pools_1],
	Pools2 = [Pools_2],
	PoolSpecs = lists:map(fun({Name, SizeArgs, WorkerArgs}) ->
		PoolArgs = [{name, {local, Name}},
			{worker_module, chronos_worker}] ++ SizeArgs,
	poolboy:child_spec(Name, PoolArgs, WorkerArgs)
	end, Pools1),
    	PoolSpecs1 = lists:map(fun({Name, SizeArgs, WorkerArgs}) ->
   		PoolArgs = [{name, {local, Name}},
                    {worker_module, chronos_queue}] ++ SizeArgs,
        poolboy:child_spec(Name, PoolArgs, WorkerArgs)
	end, Pools2),
	{ok, {{one_for_one, 10, 10}, lists:flatten(PoolSpecs ++ PoolSpecs1)}}
	catch
		E1:E2 ->
		io:format("~p~n~p~n~p~n", [E1, E2, erlang:get_stacktrace()]) 
	end.

add_jobs(Args) ->
	NewJobs = [
		{Name, lists:flatten([{state, idle}, {last, undefined}|Settings])}
	||
	{Name, Settings} <- Args],
	ets:insert(chronos, NewJobs).

remove_job(Name) ->
	ets:take(chronos, Name).

get_jobs() ->
	Key = ets:first(chronos),
	case Key of
		'$end_of_table' ->
			[];
		K -> get_jobs(K, [K])
	end.

get_jobs(K, List) ->	
	Next = ets:next(chronos, K),
	case Next of
		'$end_of_table' ->
			List;
		N -> get_jobs(N, [N|List])
	end.
