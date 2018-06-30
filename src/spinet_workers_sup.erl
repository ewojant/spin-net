%% @author wojanton

-module(spinet_workers_sup).
-behaviour(supervisor).
-export([init/1]).

-define(SERVER, ?MODULE).

%% ====================================================================
%% API functions
%% ====================================================================
-export([start_link/0,
         add_worker/0,
         add_worker/1]).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

add_worker() ->
    Id = spinet_scheduler:get_next_worker_id(),
    ChildSpec = {spinet_worker:process_name(Id),
                 {spinet_worker, start_link, [Id]},
                 permanent, 1000, worker, [spinet_worker]},
    supervisor:start_child(?SERVER, ChildSpec).

-spec add_worker(NumWorkers :: pos_integer()) -> ok.
add_worker(0) ->
    ok;
add_worker(NumWorkers) when NumWorkers > 0 ->
    add_worker(),
    add_worker(NumWorkers - 1).

%% ====================================================================
%% Behavioural functions
%% ====================================================================

%% init/1
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/supervisor.html#Module:init-1">supervisor:init/1</a>
-spec init(Args :: term()) -> Result when
    Result :: supervisor:init_returntype().
%% ====================================================================
init([]) ->
    {ok,{{one_for_one,5,10}, []}}.

%% ====================================================================
%% Internal functions
%% ====================================================================


