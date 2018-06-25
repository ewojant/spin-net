%% @author wojanton

-module(spinet_workers_sup).
-behaviour(supervisor).
-export([init/1]).

%% ====================================================================
%% API functions
%% ====================================================================
-export([start_link/1]).

start_link(NumWorkers) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, [NumWorkers]).



%% ====================================================================
%% Behavioural functions
%% ====================================================================

%% init/1
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/supervisor.html#Module:init-1">supervisor:init/1</a>
-spec init(Args :: term()) -> Result when
    Result :: supervisor:init_returntype().
%% ====================================================================
init([NumWorkers]) ->
    Children = [{spinet_worker:process_name(Id),
                {spinet_worker, start_link, [Id]},
                permanent, 1000, worker, [spinet_worker]}
                || Id <- lists:seq(1, NumWorkers)],
    {ok,{{one_for_one,0,1}, Children}}.

%% ====================================================================
%% Internal functions
%% ====================================================================


