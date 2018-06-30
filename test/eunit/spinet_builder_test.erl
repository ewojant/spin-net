%% @author wojanton

-include_lib("mockgyver/include/mockgyver.hrl").

-module(spinet_builder_test).

%% ====================================================================
%% API functions
%% ====================================================================
-export([setup/0, cleanup/1]).

spinet_builder_test_() ->
    ?WITH_MOCKED_SETUP(fun setup/0, fun cleanup/1).

setup() ->
    #{}.

cleanup(_Cfg) ->
    ok.

%% ====================================================================
%% Internal functions
%% ====================================================================
create_normal_test(_Cfg) ->
    ct:print("Starting test ~p", [?FUNCTION_NAME]),
    [begin
        ct:print("Iteration, N=~p, M=~p, Type=~p", [N, M, Type]),
        Net = spinet_builder:create_network(N, M, Type),
        ?assertEqual(N, length(Net)),
        [begin
            % take Idx-th Node from Net
            #{idx := NodeIdx, neighbours := Neighbours} = lists:nth(Idx, Net),
            ?assertEqual(Idx, NodeIdx),
            ?assertEqual(M, length(lists:usort(Neighbours))),
            PotentialNeighbours = lists:seq(1, max(NodeIdx-1, M+1)),
            % ct:print("NodeIdx=~p,~nNeighbours=~p,~nPotentialNeighbours=~p", [NodeIdx, Neighbours, PotentialNeighbours]),
            [begin
                 
                 ?assert(lists:member(NeigbourIdx, PotentialNeighbours))
             end
             || NeigbourIdx <- Neighbours]
        end
        || Idx <- lists:seq(1, N)]
     end
     || {N, M} <- [{5, 1}, {10, 9}, {100, 20}], Type <- [exn, sfn]
    ],
    ct:print("Test ~p passed", [?FUNCTION_NAME]).

create_invalid_params_test(_Cfg) ->
    ct:print("Starting test ~p", [?FUNCTION_NAME]),
    ?assertException(error, function_clause, spinet_builder:create_network(1, 1, sfn)),
    ?assertException(error, function_clause, spinet_builder:create_network(10, 11, sfn)),
    ?assertException(error, function_clause, spinet_builder:create_network(100, 1, eee)),
    ct:print("Test ~p passed", [?FUNCTION_NAME]).

