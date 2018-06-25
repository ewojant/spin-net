%% @author wojanton

-module(spinet_scheduler).
-behaviour(gen_server).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% ====================================================================
%% API functions
%% ====================================================================
-export([start_link/0,
         register_worker/1,
         add_task/1,
         task_done/2]).

-export([dump/0]).

-define(SERVER, ?MODULE).

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

register_worker(Id) ->
    gen_server:cast(?SERVER, {register, Id, self()}).

add_task(Task) ->
    gen_server:cast(?SERVER, {add_task, Task}).

task_done(Id, Result) ->
    gen_server:cast(?SERVER, {task_done, Id, Result}).

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
           results => []}}.


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

handle_cast({register, Id, Pid},
            State = #{workers := Workers0,
                      free_workers := Free}) ->
    MRef = erlang:monitor(process, Pid),
    NewState = State#{workers => Workers0#{Id => {Pid, MRef}},
                      free_workers => [Id | Free]},
    {noreply, NewState};

handle_cast({add_task, Task},
            #{free_workers := Free0,
              tasks := Tasks0} = State) ->
    case schedule_task(Task, Free0) of
        {ok, Free1} ->
            {noreply, State#{free_workers => Free1}};

        {error, _Reason} ->
            {noreply, State#{tasks => Tasks0 ++ [Task]}}
    end;

handle_cast({task_done, Id, Result}, State) ->
    NewState = handle_task_done(Id, Result, State),
    {noreply, NewState};

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
handle_task_done(Id, Result,
                 #{free_workers := Free0,
                   tasks := [],
                   results := Results0} = State) ->
    % no tasks available so just add worker to free list
    State#{free_workers => Free0 ++ [Id],
           results => [Result | Results0]};

handle_task_done(Id, Result,
                 #{free_workers := Free0,
                   tasks := [Task | RestTask] = Tasks0,
                   results := Results0} = State) ->
    % add Id of finished worker at the end of list
    Free1 = Free0 ++ [Id],
    case schedule_task(Task, Free1) of
        {ok, Free2} ->
            State#{free_workers => Free2,
                   tasks => RestTask,
                   results => [Result | Results0]};
        {error, _Reason} ->
            State#{free_workers => Free1,
                   tasks => Tasks0,
                   results => [Result | Results0]}
    end.


schedule_task(Task, []) ->
    {error, {'No free workers to schedule: ', Task}};

schedule_task(Task, [Id | RestFree]) ->
    spinet_worker:execute(Id, Task),
    {ok, RestFree}.