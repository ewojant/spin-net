%% @author Wojciech
%% @doc @todo Add description to spin_net_builder_util_test.

-include_lib("mockgyver/include/mockgyver.hrl").
-module(spinet_scheduler_test).


%% ====================================================================
%% API functions
%% ====================================================================
-export([setup/0, cleanup/1]).

spinet_scheduler_test_() ->
        ?WITH_MOCKED_SETUP(fun setup/0, fun cleanup/1).

-define(recv_nothing(Timeout),
		receive
			Msg -> {error, {received, Msg}}
		after
			Timeout -> ok
		end).

-define(recv_task(Task),
		receive
			{execute, Task} -> ok
		after 100 ->
			{error_task_not_received, Task}
		end
		).

setup() ->
	spinet_scheduler:start_link(),
	#{}.

cleanup(_Cfg) ->
	spinet_scheduler:stop(),
	ok.

%% nothing_scheduled_without_workers_test(_Cfg) ->
%% 	[spinet_scheduler:add_task({dummy_task, X}) || X <- lists:seq(1, 100)],
%% 	?recv_nothing(100).
%% 
%% one_worker_tasks_scheduled_sequentially_test(_Cfg) ->
%% 	% add worker, without tasks it should receive nothing right now
%% 	spinet_scheduler:add_worker(self()),
%% 	?recv_nothing(100),
%% 	
%% 	[begin
%% 		 Task = {dummy_task, X},
%% 		 spinet_scheduler:add_task(Task),
%% 		 ?recv_task(Task)
%% 	 end
%% 	 || X <- lists:seq(1, 50)].

%% ====================================================================
%% Internal functions
%% ====================================================================
