%%%-------------------------------------------------------------------
%% @doc spinet top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(spinet_sup).
-behaviour(supervisor).

%% API
-export([start_link/1]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link(Args) ->
    logger:info("Starting toplevel supervisor, Args=~p", [Args]),
    supervisor:start_link({local, ?SERVER}, ?MODULE, Args).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
    Scheduler = {spinet_scheduler,
                  {spinet_scheduler, start_link, []},
                  permanent, 1000, worker, [spinet_scheduler]},
    WorkersSup = {spinet_workers_sup,
                  {spinet_workers_sup, start_link, []},
                  permanent, infinity, supervisor, [spinet_workers_sup]},
    {ok, { {one_for_all, 0, 1}, [Scheduler, WorkersSup]} }.

%%====================================================================
%% Internal functions
%%====================================================================

