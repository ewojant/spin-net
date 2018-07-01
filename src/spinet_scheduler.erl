%% @author wojanton

-module(spinet_scheduler).
-behaviour(gen_server).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% ====================================================================
%% API functions
%% ====================================================================
-export([start_link/0,
         stop/0,
         get_next_worker_id/0,
         register_worker/1,
         add_task/1,
         add_task_group/1,
         task_done/3,
         get_results/0,
         get_results/1,
         get_results/2,
         clear_task_group/1]).

-export([dump/0]).

-type task() :: mfa() | function().
-type task_group_id() :: atom() | integer().
-type task_group() :: {task_group_id(), [task()]}.

-define(SERVER, ?MODULE).
-define(DEF_GRP_ID, undefined).

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

-spec stop() -> ok.
stop() ->
    gen_server:stop(?SERVER).

-spec register_worker(Id :: spinet_worker:worker_id()) -> ok.
register_worker(Id) ->
    gen_server:cast(?SERVER, {register_worker, Id, self()}).

-spec get_next_worker_id() -> spinet_worker:worker_id().
get_next_worker_id() ->
    gen_server:call(?SERVER, get_next_worker_id).

-spec add_task(Task :: task()) -> ok.
add_task(Task) ->
    gen_server:cast(?SERVER, {add_task, Task}).

-spec add_task_group(TaskGroup :: task_group()) -> ok.
add_task_group(TaskGroup) ->
    gen_server:cast(?SERVER, {add_task_group, TaskGroup}).

-spec task_done(WorkerId, TaskGroupId, Result) -> ok when
        WorkerId :: spinet_worker:worker_id(),
        TaskGroupId :: task_group_id(),
        Result :: term().
task_done(WorkerId, TaskGroupId, Result) ->
    gen_server:cast(?SERVER, {task_done, WorkerId, TaskGroupId, Result}).

-spec get_results() -> [term()].
get_results() ->
    get_results(?DEF_GRP_ID, infinity).

-spec get_results(TaskGroupId :: task_group_id()) -> [term()].
get_results(TaskGroupId) ->
    get_results(TaskGroupId, infinity).

-spec get_results(TaskGroupId, Timeout) -> Result when
        TaskGroupId :: task_group_id(),
        Timeout :: timeout(),
        Result :: [term()].
get_results(TaskGroupId, Timeout) when 
        (is_integer(Timeout) andalso Timeout >= 0) orelse
        (Timeout == infinity) ->
    gen_server:call(?SERVER, {get_results, TaskGroupId}, Timeout).

clear_task_group(TaskGroupId) ->
    gen_server:call(?SERVER, {clear_task_group, TaskGroupId}).

-spec dump() -> ok.
dump() ->
    gen_server:call(?SERVER, dump).
%% ====================================================================
%% Behavioural functions
%% ====================================================================

%% init/1
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:init-1">gen_server:init/1</a>
-spec init(Args :: term()) -> Result when
    Result :: {ok, State}
            | {ok, State, Timeout}
            | {ok, State, hibernate}
            | {stop, Reason :: term()}
            | ignore,
    State :: term(),
    Timeout :: non_neg_integer() | infinity.
%% ====================================================================
init([]) ->
    {ok, #{workers => maps:new(),
           free_workers => [],
           tasks => [],
           task_groups => #{?DEF_GRP_ID => new_task_group()}
          }
    }.

new_task_group() ->
    #{size => 0,
      tasks_done => 0,
      results => [],
      waiters => []}.

%% handle_call/3
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:handle_call-3">gen_server:handle_call/3</a>
-spec handle_call(Request :: term(), From :: {pid(), Tag :: term()}, State :: term()) -> Result when
    Result :: {reply, Reply, NewState}
            | {reply, Reply, NewState, Timeout}
            | {reply, Reply, NewState, hibernate}
            | {noreply, NewState}
            | {noreply, NewState, Timeout}
            | {noreply, NewState, hibernate}
            | {stop, Reason, Reply, NewState}
            | {stop, Reason, NewState},
    Reply :: term(),
    NewState :: term(),
    Timeout :: non_neg_integer() | infinity,
    Reason :: term().
%% ====================================================================
handle_call(dump, _From, State) ->
    io:format("~p state: ~p~n", [?MODULE, State]),
    {reply, ok, State};

handle_call({get_results, TaskGroupId}, From,
            #{task_groups := TaskGroups} = State) ->
    case maps:get(TaskGroupId, TaskGroups, undefined) of
        undefined ->
            Reply = {error, {'No such task group: ', TaskGroupId}},
            {reply, Reply, State};
        #{size := Size,
          tasks_done := TDone,
          results := Results0,
          waiters := Waiters} when Size == TDone ->
            Results = lists:reverse(Results0),
            lists:foreach(fun(Pid) -> gen_server:reply(Pid, Results) end,
                          Waiters),
            {reply, Results, State};

        TaskGroup0 = #{waiters := Waiters} ->
            TaskGroup1 = TaskGroup0#{waiters => [From | Waiters]},
            {noreply, State#{task_groups => TaskGroups#{TaskGroupId => TaskGroup1}}}
    end;

handle_call(get_next_worker_id, _From, #{workers := Workers}=State) ->
    {reply, maps:size(Workers) + 1, State};

handle_call({clear_task_group, TGI}, _From,
            #{task_groups := TaskGroups}=State) ->
    case maps:get(TGI, TaskGroups, undefined) of
        undefined ->
            {reply, ok, State};
        _TG ->
            {reply, ok, State#{task_groups => maps:remove(TGI, TaskGroups)}}
    end;

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.


%% handle_cast/2
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:handle_cast-2">gen_server:handle_cast/2</a>
-spec handle_cast(Request :: term(), State :: term()) -> Result when
    Result :: {noreply, NewState}
            | {noreply, NewState, Timeout}
            | {noreply, NewState, hibernate}
            | {stop, Reason :: term(), NewState},
    NewState :: term(),
    Timeout :: non_neg_integer() | infinity.
%% ====================================================================

handle_cast({register_worker, Id, Pid},
            State = #{workers := Workers0,
                      free_workers := Free,
                      tasks := Tasks0}) ->
    MRef = erlang:monitor(process, Pid),
    {NewTasks, NewFreeWorkers} =
        schedule_tasks(Tasks0, Free ++ [Id]),
    NewState = State#{workers => Workers0#{Id => {Pid, MRef}},
                      free_workers => NewFreeWorkers,
                      tasks => NewTasks},
    {noreply, NewState};

handle_cast({add_task, Task},
            #{free_workers := Free0,
              tasks := Tasks0,
              task_groups := TaskGroups0} = State) ->
    {NewTasks, NewWorkers} =
        schedule_tasks(Tasks0 ++ [{?DEF_GRP_ID, Task}], Free0),
    #{size := Size} = TaskGroup = maps:get(?DEF_GRP_ID, TaskGroups0),
    TaskGroupNew = TaskGroup#{size => Size + 1},
    {noreply, State#{tasks => NewTasks,
                     free_workers => NewWorkers,
                     task_groups => TaskGroups0#{?DEF_GRP_ID => TaskGroupNew}}};

handle_cast({add_task_group, {TaskGroupId, Tasks} = _TaskGroup},
            #{free_workers := Free0,
              tasks := Tasks0,
              task_groups := TaskGroups0} = State) ->
    TaskList = [{TaskGroupId, Task} || Task <- Tasks],
    {NewTasks, NewWorkers} =
        schedule_tasks(Tasks0 ++ TaskList, Free0),
    #{size := Size} = TaskGroup =
        maps:get(TaskGroupId, TaskGroups0, new_task_group()),
    TaskGroupNew = TaskGroup#{size => Size + length(Tasks)},
    {noreply, State#{tasks => NewTasks,
                     free_workers => NewWorkers,
                     task_groups => TaskGroups0#{TaskGroupId => TaskGroupNew}}};

handle_cast({task_done, Id, TaskGroupId, Result},
            #{free_workers := Workers0,
              tasks := Tasks0,
              task_groups := TaskGroups0} = State) ->
    {NewTasks, NewWorkers} = schedule_tasks(Tasks0, Workers0 ++ [Id]),
    case maps:get(TaskGroupId, TaskGroups0, undefined) of
        undefined ->
            logger:error("Task done by worker=~p for unknown task group id=~p",
                         [Id, TaskGroupId]),
            {noreply, State#{free_workers => NewWorkers,
                             tasks => NewTasks}};
        TaskGroup0 ->
            TaskGroup1 = #{size:= Size, tasks_done:=Td} = store_result(Result, TaskGroup0),
            logger:debug("Task done, TGI=~p, #~p/~p", [TaskGroupId, Td, Size]),
            {noreply, State#{free_workers => NewWorkers,
                             tasks => NewTasks,
                             task_groups => TaskGroups0#{TaskGroupId=>TaskGroup1}}}
    end;

handle_cast(_Msg, State) ->
    {noreply, State}.


%% handle_info/2
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:handle_info-2">gen_server:handle_info/2</a>
-spec handle_info(Info :: timeout | term(), State :: term()) -> Result when
    Result :: {noreply, NewState}
            | {noreply, NewState, Timeout}
            | {noreply, NewState, hibernate}
            | {stop, Reason :: term(), NewState},
    NewState :: term(),
    Timeout :: non_neg_integer() | infinity.
%% ====================================================================
handle_info({'DOWN', MonitorRef, process, Pid, _Info},
            State = #{workers := Workers,
                      free_workers := Free}) ->
    case maps:fold(fun (Id_In, {Pid_In, MRef_In}, Acc) ->
                        if Pid_In == Pid andalso MRef_In == MonitorRef ->
                                Id_In;
                            true ->
                                Acc
                        end
                    end,
                    undefined,
                    Workers)
    of
        undefined ->
            {noreply, State};
        Id when is_integer(Id) ->
            {noreply, State#{workers => maps:remove(Id, Workers),
                             free_workers => lists:delete(Id, Free)}}
    end;

handle_info(_Info, State) ->
    {noreply, State}.


%% terminate/2
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:terminate-2">gen_server:terminate/2</a>
-spec terminate(Reason, State :: term()) -> Any :: term() when
    Reason :: normal
            | shutdown
            | {shutdown, term()}
            | term().
%% ====================================================================
terminate(_Reason, _State) ->
    ok.


%% code_change/3
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:code_change-3">gen_server:code_change/3</a>
-spec code_change(OldVsn, State :: term(), Extra :: term()) -> Result when
    Result :: {ok, NewState :: term()} | {error, Reason :: term()},
    OldVsn :: Vsn | {down, Vsn},
    Vsn :: term().
%% ====================================================================
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%% ====================================================================
%% Internal functions
%% ====================================================================

schedule_tasks(Tasks, Workers) when Tasks == [];
                                    Workers == [] ->
    {Tasks, Workers};

schedule_tasks([Task | RestTask], [WorkerId | RestWorker]) ->
    spinet_worker:execute(WorkerId, Task),
    schedule_tasks(RestTask, RestWorker).

store_result(Result,
             #{size := Size,
               tasks_done := Done,
               results := Results0,
               waiters := Waiters} = TaskGroup) ->
    Done1 = Done + 1,
    Results1 = [Result | Results0],
    if Size == Done1 ->
        lists:foreach(fun(Pid) ->
                        gen_server:reply(Pid, lists:reverse(Results1))
                      end,
                      Waiters);
        true ->
            ok
    end,
    TaskGroup#{results => Results1,
               tasks_done => Done1}.
