%% @author wojanton

-module(spinet_scheduler).
-behaviour(gen_server).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% ====================================================================
%% API functions
%% ====================================================================
-export([start_link/0,
		 register_worker/1,
		 worker_done/1]).

-define(SERVER, ?MODULE).

start_link() ->
	gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

register_worker(Id) ->
	gen_server:cast(?SERVER, {register, Id, self()}).

worker_done(Id) ->
	gen_server:cast(?SERVER, {worker_done, Id}).

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
		   tasks => []}}.


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

handle_cast({worker_done, Id}, State) ->
	NewState = handle_worker_done(Id, State),
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
handle_worker_done(Id, State = #{free_workers := Free,
					             tasks := []}) ->
	% no tasks available so just add worker to free list
	State#{free_workers => Free ++ [Id]};

handle_worker_done(Id, State = #{free_workers := Free0,
					             tasks := [Task | RestTask]}) ->
	Free1 = schedule_task(Task, Free0 ++ [Id]),
	State#{free_workers => Free1,
		   tasks => RestTask}.

schedule_task(Task, [Id | RestFree]) ->
	spinet_worker:execute(Id, Task),
	RestFree.