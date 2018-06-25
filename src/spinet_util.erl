%% @author wojanton
%% @doc Utility functions for creating networks.

-module(spinet_util).

%% ====================================================================
%% API functions
%% ====================================================================
-export([unique_seq/2,
         unique_select/2]).

unique_seq(Size, MaxVal) when MaxVal >= Size ->
    unique_seq(Size, MaxVal, []).

unique_select(List, Number) when is_list(List),
                                 length(List) >= Number ->
    unique_select(List, length(List), Number, []).

%% ====================================================================
%% Internal functions
%% ====================================================================

unique_seq(0, _MaxVal, Seq) ->
    lists:sort(Seq);

unique_seq(Size, MaxVal, Seq) ->
    Val = rand:uniform(MaxVal),
    case lists:member(Val, Seq) of
        true ->
            unique_seq(Size, MaxVal, Seq);
        false ->
            unique_seq(Size - 1, MaxVal, [Val | Seq])
    end.

unique_select(_List, _ListSize, 0, Selected) ->
    Selected;

unique_select(List, ListSize, Number, Selected) ->
    Val = lists:nth(rand:uniform(ListSize), List),
    case lists:member(Val, Selected) of
        true ->
            unique_select(List, ListSize, Number, Selected);
        false ->
            unique_select(List, ListSize, Number - 1, Selected ++ [Val])
    end.