%% @author Wojciech
%% @doc @todo Add description to spin_net_builder_util_test.

-include_lib("mockgyver/include/mockgyver.hrl").
-module(spinet_util_test).


%% ====================================================================
%% API functions
%% ====================================================================
-export([setup/0, cleanup/1]).

spinet_util_test_() ->
    ?WITH_MOCKED_SETUP(fun setup/0, fun cleanup/1).

setup() ->
	#{}.

cleanup(_Cfg) ->
	ok.

%% ====================================================================
%% Internal functions
%% ====================================================================

unique_sequence_test(_Cfg) ->
	[begin 
		Seq = spinet_util:unique_seq(Size, MaxVal),
		% usort removes duplicates so this will check if there were any
		?assertEqual(Size, length(lists:usort(Seq))),
		?assert(MaxVal >= lists:max(Seq))
	 end
	 || {Size, MaxVal} <- [{5, 20}, {100, 1000}, {500, 500}, {20, 10000}]].

unique_sequence_negative_test(_Cfg) ->
	?assertError(function_clause, spinet_util:unique_seq(10, 2)),
	?assertError(function_clause, spinet_util:unique_seq(100, 99)).

unique_select_test(_Cfg) ->
	[begin 
		List = spinet_util:unique_select(InList, NumElemsToSelect),
		% usort removes duplicates so this will check if there were any
		?assertEqual(NumElemsToSelect, length(lists:usort(List))),
		?assertEqual([], lists:dropwhile(fun(Elem) ->
										     lists:member(Elem, InList)
 									     end,
 									     List))
	 end
	 || {InList, NumElemsToSelect} <- [{lists:seq(1, 50), 10},
									   {lists:seq(5, 10), 3},
									   {lists:seq(1, 1000), 500},
									   {[1,1,1,1,1,1,2,3,4,5], 5}]].

unique_select_negative_test(_Cfg) ->
	?assertError(function_clause, spinet_util:unique_select([50], 2)),
	?assertError(function_clause, spinet_util:unique_select(100, 99)).
