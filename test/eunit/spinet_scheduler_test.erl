%% @author wojanton

-include_lib("mockgyver/include/mockgyver.hrl").
-module(spinet_scheduler_test).

%% ====================================================================
%% API functions
%% ====================================================================
-export([setup/0, cleanup/1]).

spinet_scheduler_test_() ->
    ?WITH_MOCKED_SETUP(fun setup/0, fun cleanup/1).

-define(TASK(X), {dummy_module, dummy_fun, [X]}).

setup() ->
    ?WHEN(dummy_module:dummy_fun(X) -> X),
    spinet_scheduler:start_link(),
    #{}.

cleanup(_Cfg) ->
    spinet_scheduler:stop(),
    ok.

nothing_scheduled_without_workers_test(_Cfg) ->
    ct:print("Starting test ~p", [?FUNCTION_NAME]),
    [spinet_scheduler:add_task(?TASK(X))
     || X <- lists:seq(1, 10)],

    ?WAS_CALLED(dummy_module:dummy_fun(_), never),
    ct:print("Test ~p passed", [?FUNCTION_NAME]).

one_worker_tasks_scheduled_sequentially_test(_Cfg) ->
    ct:print("Starting test ~p", [?FUNCTION_NAME]),
    WorkerId = 1,
    {ok, Pid} = spinet_worker:start_link(WorkerId),
    spinet_scheduler:register_worker(WorkerId),
    ArgsList = lists:seq(1, 50),
    [spinet_scheduler:add_task(?TASK(X)) || X <- ArgsList],
    timer:sleep(1000),
    Results = spinet_scheduler:get_results(),
    ?assertEqual(ArgsList, Results),
    unlink(Pid),
    exit(Pid, shutdown),
    ct:print("Test ~p passed", [?FUNCTION_NAME]).

schedule_task_group_test(_Cfg) ->
    ct:print("Starting test ~p", [?FUNCTION_NAME]),
    WorkerId = 1,
    {ok, Pid} = spinet_worker:start_link(WorkerId),
    spinet_scheduler:register_worker(WorkerId),
    TaskGroupId = eeee,
    ArgsList = lists:seq(1, 10),
    TaskGroup = {TaskGroupId, [?TASK(X) || X <- ArgsList]},
    spinet_scheduler:add_task_group(TaskGroup),
    timer:sleep(500),
    Results = spinet_scheduler:get_results(TaskGroupId),
    ?assertEqual(ArgsList, Results),
    unlink(Pid),
    exit(Pid, shutdown),
    ct:print("Test ~p passed", [?FUNCTION_NAME]).

multiple_workers_test(_Cfg) ->
    ct:print("Starting test ~p", [?FUNCTION_NAME]),
    WorkerPids = [
        begin
            {ok, Pid} = spinet_worker:start_link(WorkerId),
            spinet_scheduler:register_worker(WorkerId),
            Pid
        end
        || WorkerId <- lists:seq(1, 10)],
    ArgsList = lists:seq(1, 1000),
    TaskGroupId = "whatever",
    TaskGroup = {TaskGroupId, [?TASK(X) || X <- ArgsList]},
    spinet_scheduler:add_task_group(TaskGroup),
    timer:sleep(1000),
    Results = spinet_scheduler:get_results(TaskGroupId),
    ?assert(compare_lists(ArgsList, Results)),
    [begin unlink(Pid), exit(Pid, shutdown) end || Pid <- WorkerPids],
    ct:print("Test ~p passed", [?FUNCTION_NAME]).

%% ====================================================================
%% Internal functions
%% ====================================================================
compare_lists([], _) ->
    true;
compare_lists([H | Tail], Other) ->
    case lists:member(H, Other) of
        true ->
            compare_lists(Tail, lists:delete(H, Other));
        false ->
            false
    end.