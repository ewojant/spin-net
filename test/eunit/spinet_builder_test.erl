%% @author Wojciech
%% @doc @todo Add description to spin_net_builder_util_test.

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

initial_network_test(_Cfg) ->
	MaxM = 1000,
	Repeats = 100,
	M_list = [1,2,5 | [rand:uniform(MaxM) || _X <- lists:seq(1, Repeats)]],
	[begin
		 Net = spinet_builder:initial_network(M),
		 ?assertEqual(M + 1, length(Net)),
		 [begin
			  ?assertNot(lists:member(Idx, Neighbours)),
			  ?assertEqual(M, length(Neighbours))
		  end
		  || #{idx := Idx, neighbours := Neighbours} <- Net]
     end
	 || M <- M_list].

create_exn_normal_test(_Cfg) ->
	[begin
		Net = spinet_builder:create_network(N, M, exn),
		?assertEqual(N, length(Net)),
		[begin
			 % take Idx-th Node from Net
			 #{idx := NodeIdx, neighbours := Neighbours} = lists:nth(Idx, Net),
			 ?assertEqual(Idx, NodeIdx),
			 ?assertEqual(M, length(lists:usort(Neighbours))),
			 PotentialNeighbours = lists:delete(NodeIdx, lists:seq(1, N)),
			 [?assert(lists:member(NeigbourIdx, PotentialNeighbours)) || NeigbourIdx <- Neighbours]
		 end
		 || Idx <- lists:seq(1, N)]
	 end
	 || {N, M} <- [{5, 1}, {10, 9}, {1000, 50}]
	].