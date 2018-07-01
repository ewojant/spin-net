%% @author wojanton

-module(spinet_network).

%% ====================================================================
%% API functions
%% ====================================================================
-export([create/3,
         create/4,
         get/2]).

create(N, M, Type) when N > 2,
                        M < N,
                        (Type == sfn orelse Type == exn) ->
    create(N, M, Type, #{}).

create(N, M, Type, InitData) when N > 2,
                             M < N,
                             (Type == sfn orelse Type == exn) ->
    InitialNetwork = initial_network(M, InitData),
    create_aux(N, M, Type, InitData,
               InitialNetwork, length(InitialNetwork) + 1).

get(Idx, Net) ->
    {value, Node} = lists:search(fun (#{idx := I}) when I == Idx -> true;
                                     (_) -> false end, Net),
    Node.

%% ====================================================================
%% Internal functions
%% ====================================================================

initial_network(M, InitData) ->
    Indexes = lists:seq(1, M + 1),
    [begin
            Neighbours = lists:delete(Idx, Indexes),
            #{idx => Idx,
              neighbours => Neighbours,
              data => InitData}
     end
     || Idx <- Indexes].

create_aux(N, M, Type, _InitData, Network, NextIdx) when NextIdx > N ->
    logger:debug("builder - finished net: N=~w, M=~w, type=~w",
                 [N, M, Type]),
    Network;

create_aux(N, M, Type, InitData, Net, NextIdx) ->
    SelectedNeighbours = case Type of
        exn ->
            spinet_util:unique_seq(M, NextIdx - 1);
        sfn ->
            find_sfn_neighbours(Net, M)
    end,
    NewNode = #{idx => NextIdx,
                neighbours => SelectedNeighbours,
                data => InitData
               },
    % Each of nodes selected as neighbour must have the newly created node
    % add as their neighbour as well
    NewNet = update_neighbours(NextIdx, SelectedNeighbours, Net),
    create_aux(N, M, Type, InitData, NewNet ++ [NewNode], NextIdx + 1).

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