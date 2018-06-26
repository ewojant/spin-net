%% @author wojanton

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

-define(TASK(X), {dummy_module, dummy_fun, [X]}).

setup() ->
    ?WHEN(dummy_module:dummy_fun(X) -> X),
    spinet_scheduler:start_link(),
    #{}.

cleanup(_Cfg) ->
    spinet_scheduler:stop(),
    ok.

nothing_scheduled_without_workers_test(_Cfg) ->
    [spinet_scheduler:add_task(?TASK(X))
     || X <- lists:seq(1, 10)],
    % ?recv_nothing(500),
    ?WAS_CALLED(dummy_module:dummy_fun(_), never).

one_worker_tasks_scheduled_sequentially_test(_Cfg) ->
    WorkerId = 1,
    {ok, Pid} = spinet_worker:start_link(WorkerId),
    % add worker, without tasks it should receive nothing right now
    spinet_scheduler:register_worker(WorkerId),
    Args = lists:seq(1, 50),
    [spinet_scheduler:add_task(?TASK(X)) || X <- Args],
    timer:sleep(1000),
    Results = spinet_scheduler:get_results(),
    ?assertEqual(Args, Results),
    unlink(Pid),
    exit(Pid, shutdown).

%% ====================================================================
%% Internal functions
%% ====================================================================
