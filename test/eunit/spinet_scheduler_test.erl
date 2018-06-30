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
-define(TASK_DELAYED(X, T), {dummy_module, dummy_fun, [X, T]}).

setup() ->
    ?WHEN(dummy_module:dummy_fun(X) -> X),
    ?WHEN(dummy_module:dummy_fun(X, T) ->
        begin timer:sleep(T), X end
    ),
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
    ?WAS_CALLED(dummy_module:dummy_fun(_, _), never),
    ct:print("Test ~p passed", [?FUNCTION_NAME]).

one_worker_tasks_scheduled_sequentially_test(_Cfg) ->
    ct:print("Starting test ~p", [?FUNCTION_NAME]),
    WorkerId = 1,
    {ok, Pid} = spinet_worker:start_link(WorkerId),
    spinet_scheduler:register_worker(WorkerId),
    ArgsList = lists:seq(1, 50),
    [spinet_scheduler:add_task(?TASK(X)) || X <- ArgsList],

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

    Results = spinet_scheduler:get_results(TaskGroupId),
    ?assert(compare_lists(ArgsList, Results)),
    [begin unlink(Pid), exit(Pid, shutdown) end || Pid <- WorkerPids],
    ct:print("Test ~p passed", [?FUNCTION_NAME]).

multiple_waiters_test(_Cfg) ->
    ct:print("Starting test ~p", [?FUNCTION_NAME]),
    WorkerPids = [
        begin
            {ok, Pid} = spinet_worker:start_link(WorkerId),
            spinet_scheduler:register_worker(WorkerId),
            Pid
        end
        || WorkerId <- lists:seq(1, 5)],
    ArgsList = lists:seq(1, 20),
    TaskGroupIds = ["whatever", eeee, 888],
    TaskGroups = [{TaskGroupId, [?TASK_DELAYED(X, 50) || X <- ArgsList]}
                  || TaskGroupId <- TaskGroupIds],
    [spinet_scheduler:add_task_group(TG) || TG <- TaskGroups],

    [result_waiter(TGI) || TGI <- TaskGroupIds],
    Results = wait_for_results(TaskGroupIds, []),

    [compare_lists(ArgsList, R) || R <- Results],

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

result_waiter(TaskGroupId) ->
    ReplyTo = self(),
    spawn(fun() ->
              Result = spinet_scheduler:get_results(TaskGroupId),
              ReplyTo ! {TaskGroupId, Result}
          end).

wait_for_results([], ResultAcc) ->
    ResultAcc;
wait_for_results(TaskGroupIds, ResultAcc) ->
    receive
        {TaskGroupId, Result} ->
            wait_for_results(lists:delete(TaskGroupId, TaskGroupIds),
                             [Result | ResultAcc])
    end.