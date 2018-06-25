%% @author wojanton

-module(spinet_builder).

%% ====================================================================
%% API functions
%% ====================================================================
-export([initial_network/2,
         create_network/3,
         create_network/4]).

create_network(N, M, Type) when N > 2,
                                M < N,
                               (Type == sfn orelse Type == exn) ->
    create_network(N, M, Type, #{}).


create_network(N, M, Type, InitData)
        when N > 2,
             M < N,
             (Type == sfn orelse Type == exn) ->
    InitialNetwork = initial_network(M, InitData),
    create_network_aux(N, M, Type, InitData,
                       InitialNetwork,
                       length(InitialNetwork) + 1).

initial_network(M, InitData) ->
    Indexes = lists:seq(1, M + 1),
    [begin
            Neighbours = lists:delete(Idx, Indexes),
            #{idx => Idx,
              neighbours => Neighbours,
              data => InitData}
     end
     || Idx <- Indexes].

%% ====================================================================
%% Internal functions
%% ====================================================================

create_network_aux(N, M, Type, _InitData, Network, NextIdx) when NextIdx > N ->
    logger:debug("builder - finished net: N=~w, M=~w, type=~w, NextIdx=~w",
                 [N, M, Type, NextIdx]),
    Network;

create_network_aux(N, M, exn, InitData, Net, NextIdx) ->
    NewNode = #{idx => NextIdx,
                neighbours => spinet_util:unique_seq(M, NextIdx - 1),
                data => InitData
               },
    create_network_aux(N, M, exn, InitData, Net ++ [NewNode], NextIdx + 1);

create_network_aux(N, M, sfn, InitData, Net, NextIdx) ->
    NewNode = #{idx => NextIdx,
                neighbours => find_sfn_neighbours(Net, M),
                data => InitData},
    create_network_aux(N, M, sfn, InitData, Net ++ [NewNode], NextIdx + 1).

find_sfn_neighbours(Network, M) ->
    % create a list which contains each index 'i' as many times
    % as the number of neighbours the 'i-th' node has. So the more
    % neighbours a node has the bigger his representation on this list is.
    SearchList = lists:foldl(
        fun(#{idx := Idx, neighbours := Neighbours}, AccList) ->
             % put Idx into list as many times as this node has neighbours
            lists:duplicate(length(Neighbours), Idx) ++ AccList
        end,
        [],
        Network),
    % select M unique values from this list
    spinet_util:unique_select(SearchList, M).
