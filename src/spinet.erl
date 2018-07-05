%% @author wojanton

-module(spinet).
-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).
-export([degree/4]).

start(_StartType, StartArgs) ->
    logger:set_primary_config(level, debug),
    logger:info("Starting spinet, Args=~p", [StartArgs]),
    Ret = spinet_sup:start_link(StartArgs),
    spinet_workers_sup:add_worker(20),
    Ret.

stop(_State) -> ok.

%% ====================================================================
%% API functions
%% ====================================================================

degree(N, M, Type, Number) ->
    TG = {degree_dist_calc,
          [fun() -> degree_dist(N, M, Type) end || _X <- lists:seq(1, Number)]},

    spinet_scheduler:add_task_group(TG),
    Results = spinet_scheduler:get_results(degree_dist_calc),
    spinet_scheduler:clear_task_group(degree_dist_calc),
    % Go over results from all tasks and create a mapping
    % node degree => number of occurences
    DistSum = lists:foldl(
        fun(Result, Acc) ->
            maps:fold(fun(D, V, AccIn) ->
                          AccIn#{D => maps:get(D, AccIn, 0) + V}
                      end,
                      Acc,
                      Result)
        end,
        #{},
        Results),
    % divide number of occurences of each degree by number of
    % nodes in each network and number of networks
    Dist = maps:map(fun(_K, V) -> V / (N * Number) end, DistSum),
    io:format("Degree distribution: ~p~n", [Dist]),
    ok.

%% ====================================================================
%% Internal functions
%% ====================================================================

degree_dist(N, M, Type) ->
    Net = spinet_network:create(N, M, Type),
    % io:format("Net: ~p~n", [Net]),
    Result = lists:foldl(
        fun(#{neighbours := Neighbours} = _Node, Acc) ->
            NeighboursLen = length(Neighbours),
            Acc#{NeighboursLen => maps:get(NeighboursLen, Acc, 0) + 1}
        end,
        #{},
        Net
    ),
    Result.
