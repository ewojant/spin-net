%% @author wojanton

-module(spinet_builder).

%% ====================================================================
%% API functions
%% ====================================================================
-export([create_network/3,
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
    SelectedNeighbours = spinet_util:unique_seq(M, NextIdx - 1),
    NewNode = #{idx => NextIdx,
                neighbours => SelectedNeighbours,
                data => InitData
               },
    % Each of nodes selected as neighbour must have the newly created node
    % add as their neighbour as well
    NewNet = update_neighbours(NextIdx, SelectedNeighbours, Net),
    create_network_aux(N, M, exn, InitData, NewNet ++ [NewNode], NextIdx + 1);

create_network_aux(N, M, sfn, InitData, Net, NextIdx) ->
    SelectedNeighbours = find_sfn_neighbours(Net, M),
    NewNode = #{idx => NextIdx,
                neighbours => SelectedNeighbours,
                data => InitData},
    % Each of nodes selected as neighbour must have the newly created node
    % add as their neighbour as well
    NewNet = update_neighbours(NextIdx, SelectedNeighbours, Net),
    create_network_aux(N, M, sfn, InitData, NewNet ++ [NewNode], NextIdx + 1).

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

update_neighbours(IdxToAdd, SelectedNeighbours, Net) ->
    lists:foldl(
        fun(Idx, NetAcc) ->
            % io:format("~p Looking for node idx=~p~n", [self(), Idx]),
            {value, Node = #{neighbours := N0}} = lists:search(
                fun(#{idx := Index}) when Index == Idx -> true;
                    (#{}) -> false
                end,
                NetAcc),
            % io:format("~p Found node=~p~n", [self(), Node]),
            NewNode = Node#{neighbours => [IdxToAdd | N0]},
            [NewNode | lists:delete(Node, NetAcc)]
        end,
        Net,
        SelectedNeighbours).