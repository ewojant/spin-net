%% @author wojanton
%% @doc Utility functions for creating networks.

-module(spinet_util).

%% ====================================================================
%% API functions
%% ====================================================================
-export([unique_seq/2,
         unique_select/2]).

unique_seq(Size, MaxVal) when MaxVal >= Size ->
    unique_seq(Size, MaxVal, sets:new()).

unique_select(List, Number) when is_list(List),
                                 length(List) >= Number ->
    unique_select(List, Number, sets:new()).

%% ====================================================================
%% Internal functions
%% ====================================================================

unique_seq(0, _MaxVal, Set) ->
    lists:sort(sets:to_list(Set));

unique_seq(Size, MaxVal, Set) ->
    Val = rand:uniform(MaxVal),
    case sets:is_element(Val, Set) of
        true ->
            unique_seq(Size, MaxVal, Set);
        false ->
            unique_seq(Size - 1, MaxVal, sets:add_element(Val, Set))
    end.

unique_select(_List, 0, Selected) ->
    sets:to_list(Selected);

unique_select(List, Number, Selected) ->
    Val = lists:nth(rand:uniform(length(List)), List),
    case sets:is_element(Val, Selected) of
        true ->
            unique_select(List, Number, Selected);
        false ->
            ListNew = lists:filter(
                fun(Elem) when Elem == Val -> false;
                    (_) -> true end,
                List
            ),
            unique_select(ListNew, Number - 1, sets:add_element(Val, Selected))
    end.