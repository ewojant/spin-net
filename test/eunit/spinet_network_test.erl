%% @author wojanton

-include_lib("mockgyver/include/mockgyver.hrl").

-module(spinet_network_test).

%% ====================================================================
%% API functions
%% ====================================================================
-export([setup/0, cleanup/1]).

spinet_network_test_() ->
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
        Net = spinet_network:create(N, M, Type),
        ?assertEqual(N, length(Net)),
        [begin
            % take Idx-th Node from Net
            #{idx := Idx, neighbours := Neighbours} = spinet_network:get(Idx, Net),
            % ct:print("Idx=~p, len(Neigh)=~p, neighbours=~p", [Idx, length(Neighbours), Neighbours]),
            ?assert(length(lists:usort(Neighbours)) >= M)
        end
        || Idx <- lists:seq(1, N)]
     end
     || {N, M} <- [{5, 1}, {10, 9}, {100, 20}], Type <- [exn, sfn]
    ],
    ct:print("Test ~p passed", [?FUNCTION_NAME]).

create_invalid_params_test(_Cfg) ->
    ct:print("Starting test ~p", [?FUNCTION_NAME]),
    ?assertException(error, function_clause, spinet_network:create(1, 1, sfn)),
    ?assertException(error, function_clause, spinet_network:create(10, 11, sfn)),
    ?assertException(error, function_clause, spinet_network:create(100, 1, eee)),
    ct:print("Test ~p passed", [?FUNCTION_NAME]).

