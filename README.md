# chronos
Scheduler for Erlang.

It's kind of cron on Linux systems. Configuration file also looks like as in cron.
But there is slight difference. Seconds was added to format of configuration file.

Example of config file (sys.config):

```erlang
{chronos,
	[{jobs, 
		[
		 	{"job1",
				[
					{time,	{"*/15","*","*","7","*","*","*"}}, % every 15 seconds on 7th day of month
					{max_retries, 0},
					{period_restart, 0},
					{mfa, {module,job1,[arg1, arg2, <<"arg3">>]}}
				]
			},
			{"job2",
				[
					{time,	{"0","10-20","*","*","*","*","*"}}, % one time in minute from 10th to 20th minute every hour
					{max_retries, 0},
					{period_restart, 0},
					{mfa, {module,job2,[]}}
				]
			}
			,
			{"job3",
				[
					{time,	{"*/20","*","*","*","12","5","2017"}}, every friday in December 2017
					{max_retries, 0},
					{period_restart, 0},
					{mfa, {module,job3,[]}}
				]
			}
		]
	},
	{pools,
		[
           	{chronos_worker, 
			 	[
        	        {size, 40},
    	            {max_overflow, 60}
	            ], 
				[
            	]
			},
			{chronos_queue, 
			 	[
        	        {size, 1},
    	            {max_overflow, 1}
	            ], 
				[
				]
			}
		]
	},
	{settings,
		[
			{debug_print, on}
			%% in future releases
			%{email, <<"">>},
			%{email_server, <<"">>},
			%{email_login, <<"">>},
			%{email_password, <<"">>},
			%{send_email_on_crash, off}
		]
	}
	]
}
```
