%% @author wojanton

-module(spinet_network).

%% ====================================================================
%% API functions
%% ====================================================================
-export([create/3,
         create/4,
         get/2,
         add/2]).

-type network() :: list(any()).
-type net_type() :: sfn | exn.
-type net_idx() :: pos_integer().
-type net_node() :: #{idx := net_idx(),
                      neighbours := list(net_idx()),
                      data := any()}.

-spec create(N, M, Type) -> Result when
        N :: pos_integer(),
        M :: pos_integer(),
        Type :: net_type(),
        Result :: network().
create(N, M, Type) when N > 2,
                        M < N,
                        (Type == sfn orelse Type == exn) ->
    create(N, M, Type, #{}).

-spec create(N, M, Type, InitData) -> Result when
        N :: pos_integer(),
        M :: pos_integer(),
        Type :: net_type(),
        InitData :: any(),
        Result :: network().
create(N, M, Type, InitData) when N > 2,
                             M < N,
                             (Type == sfn orelse Type == exn) ->
    InitialNetwork = initial_network(M, InitData),
    create_aux(N, M, Type, InitData,
               InitialNetwork, length(InitialNetwork) + 1).

-spec get(Idx :: net_idx(), Net :: network()) -> net_node().
get(Idx, Net) ->
    case lists:search(fun (#{idx := I}) when I == Idx -> true;
                          (_) -> false end, Net) of
        {value, Node} ->
            Node;
        false ->
            undefined
    end.

-spec add(Node :: net_node(), Net :: network()) -> network().
add(Node, Net) ->
    Net ++ [Node].

-spec delete(Node :: net_node(), Net :: network()) -> network().
delete(Node, Net) ->
    lists:delete(Node, Net).

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

create_aux(N, M, Type, _InitData, Network, IdxToCreate) when IdxToCreate > N ->
    logger:debug("builder - finished net: N=~w, M=~w, type=~w",
                 [N, M, Type]),
    Network;

create_aux(N, M, Type, InitData, Net, IdxToCreate) ->
    SelectedNeighbours = case Type of
        exn ->
            spinet_util:unique_seq(M, IdxToCreate - 1);
        sfn ->
            find_sfn_neighbours(Net, M)
    end,
    NewNode = #{idx => IdxToCreate,
                neighbours => SelectedNeighbours,
                data => InitData
               },
    % Each of nodes selected as neighbour must have the newly created node
    % added as their neighbour as well
    NewNet = update_neighbours(IdxToCreate, SelectedNeighbours, Net),
    create_aux(N, M, Type, InitData, add(NewNode, NewNet), IdxToCreate + 1).

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
            Node = #{neighbours := N0} = spinet_network:get(Idx, NetAcc),
            NewNode = Node#{neighbours => [IdxToAdd | N0]},
            add(NewNode, delete(Node, NetAcc))
        end,
        Net,
        SelectedNeighbours).