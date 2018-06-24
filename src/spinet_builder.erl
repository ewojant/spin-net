%% @author wojanton

-module(spinet_builder).

-export([initial_network/1]).

%% ====================================================================
%% API functions
%% ====================================================================
-export([create_network/3]).

create_network(N, M, Type) when N > 2,
								M < N,
								(Type == sfn orelse Type == exn) ->
	InitialNetwork = initial_network(M),
	create_network_aux(N, M, Type, InitialNetwork, length(InitialNetwork) + 1).

%% ====================================================================
%% Internal functions
%% ====================================================================

initial_network(M) ->
	Indexes = lists:seq(1, M + 1),
	[begin
		 Neighbours = lists:delete(Idx, Indexes),
		 #{idx => Idx,
		   neighbours => Neighbours}
	 end
	 || Idx <- Indexes].



create_network_aux(N, M, Type, Network, NextIdx) when NextIdx > N ->
	logger:info("builder - finished net: N=~w, M=~w, type=~w, NextIdx=~w", [N, M, Type, NextIdx]),
	Network;

create_network_aux(N, M, exn, Net, NextIdx) ->
	NewNode = #{idx => NextIdx,
				neighbours => spinet_util:unique_seq(M, NextIdx - 1)
			   },
	create_network_aux(N, M, exn, Net ++ [NewNode], NextIdx + 1);

create_network_aux(N, M, sfn, Net, NextIdx) ->
	NewNode = #{idx => NextIdx,
				neighbours => find_sfn_neighbours(Net, M)},
	create_network_aux(N, M, sfn, Net ++ [NewNode], NextIdx + 1).

find_sfn_neighbours(Network, M) ->
	SearchList =
		lists:foldl(
		  fun(#{idx:= Idx, neighbours := Neighbours}, AccList) ->
			  % put Idx into list as many times as this node has neighbours
			  [lists:duplicate(length(Neighbours), Idx) | AccList]
		  end,
		  [],
		  Network),
	spinet_util:unique_select(SearchList, M).
