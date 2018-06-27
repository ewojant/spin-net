%% @author wojanton

-module(spinet_scheduler).
-behaviour(gen_server).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% ====================================================================
%% API functions
%% ====================================================================
-export([start_link/0,
         stop/0,
         register_worker/1,
         add_task/1,
         add_task_group/1,
         task_done/3,
         get_results/0,
         get_results/1]).

-export([dump/0]).

-type task() :: mfa().
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
    gen_server:call(?SERVER, {get_results, ?DEF_GRP_ID}).

-spec get_results(TaskGroupId :: task_group_id()) -> [term()].
get_results(TaskGroupId) ->
    gen_server:call(?SERVER, {get_results, TaskGroupId}).


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
           results => #{?DEF_GRP_ID => []}}}.


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

handle_call({get_results, TaskGroupId}, _From,
            #{results := ResultsMap} = State) ->
    case maps:get(TaskGroupId, ResultsMap, undefined) of
        undefined ->
            Reply = {error, {'No results for task group with id: ', TaskGroupId}},
            {reply, Reply, State};
        Results ->
            {reply, lists:reverse(Results), State}
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
              tasks := Tasks0} = State) ->
    {NewTasks, NewWorkers} =
        schedule_tasks(Tasks0 ++ [{?DEF_GRP_ID, Task}], Free0),
    {noreply, State#{tasks => NewTasks,
                     free_workers => NewWorkers}};

handle_cast({add_task_group, {TaskGroupId, Tasks} = _TaskGroup},
            #{free_workers := Free0,
              tasks := Tasks0} = State) ->
    TaskList = [{TaskGroupId, Task} || Task <- Tasks],
    {NewTasks, NewWorkers} =
        schedule_tasks(Tasks0 ++ TaskList, Free0),
    {noreply, State#{tasks => NewTasks,
                     free_workers => NewWorkers}};

handle_cast({task_done, Id, TaskGroupId, Result},
            #{free_workers := Workers0,
              tasks := Tasks0,
              results := Results0} = State) ->
    {NewTasks, NewWorkers} = schedule_tasks(Tasks0, Workers0 ++ [Id]),
    NewResults = store_result(Result, TaskGroupId, Results0),

    {noreply, State#{free_workers => NewWorkers,
                     tasks => NewTasks,
                     results => NewResults}};

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

store_result(Result, TaskGroupId, Results) ->
    ResultsForGroup = maps:get(TaskGroupId, Results, []),
    Results#{TaskGroupId => [Result | ResultsForGroup]}.
