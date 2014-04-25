-module(newSplit).
-export([splitLists/3]).

-include_lib("eunit/include/eunit.hrl").

-spec splitLists(A, B, N) -> list() when
      A::list(),
      B::list(),
      N::integer().
splitLists(A, B, X) when X =< 0 ->
    splitLists(A, B, 1);
splitLists([], [], N) ->
    splitLists([0], [0], N);
splitLists([], B, N)->
    splitLists([0], B, N);
splitLists(A, [], N) ->
    splitLists(A, [0], N);
splitLists(A, B, N) ->
    if 
	length(A) >= length(B) ->
	    NewB = addZeros(length(A), B),
	    NewA = A;
	length(A) =< length(B) ->
	    NewA = addZeros(length(B), A),
	    NewB = B;
	length(A) == length(B) ->
	    NewA = A,
	    NewB = B
    end,
    if 
	length(NewA) div N == 0 ->
	    ALists = splitLists(NewA, 1, [], length(NewA)),
	    BLists = splitLists(NewB, 1, [], length(NewA));	
	length(NewA) div N /= 0 ->
	    LengthOfSublists = length(NewA) div N,
	    ALists = splitLists(NewA, LengthOfSublists, [], N),
	    BLists = splitLists(NewB, LengthOfSublists, [], N)
    end,
    lists:zip(ALists, BLists).


%doc splits List into sublists where all elements are still in order and 
%the first n-1 sublists are of length Length, and the last sublist contains all
%remaining elements from List
-spec splitLists(List, Length, ListList, N) -> list() when
      List::list(integer()),
      Length::integer(),
      ListList::list(list(integer())),
      N::integer().

splitLists(List, _Length, ListList, 1) ->
    lists:reverse([List | ListList]);
splitLists(List, Length, ListList, N) ->
    {First, Second} = lists:split (Length, List),
    splitLists(Second, Length, [First | ListList], N-1).


-spec addZeros(N, List) -> list() when
      N::integer(),
      List::list().

addZeros(N, List) when length(List) == N ->  
    List;
addZeros(N, List) ->
    addZeros(N, [0|List]).

%% EUnit

split_lists_test_() ->
    ?_assertEqual(splitLists([1,2,3,4,5,6,7],[1,2,3],2),
		  [{[1,2,3],[0,0,0]},{[4,5,6,7],[0,1,2,3]}]),
    ?_assertEqual(splitLists([1,2,3,4,5,6,7,8],[1],8),
		  [{[1],[0]},
		   {[2],[0]},
		   {[3],[0]},
		   {[4],[0]},
		   {[5],[0]},
		   {[6],[0]},
		   {[7],[0]},
		   {[8],[1]}]),
    ?_assertEqual(splitLists([1,2],[1,2,3,4,5,6,7],1),
		  [{[0,0,0,0,0,1,2],[1,2,3,4,5,6,7]}]),
    ?_assertEqual(splitLists([1],[2],0), 
		  [{[1],[2]}]),
    ?_assertEqual(splitLists([],[1,2,3,4,5],5),
		  [{[0],[1]},{[0],[2]},{[0],[3]},{[0],[4]},{[0],[5]}]),
    ?_assertEqual(splitLists([1,2,3,4,5],[],5),
		  [{[1],[0]},{[2],[0]},{[3],[0]},{[4],[0]},{[5],[0]}]),
    ?_assertEqual(splitLists([],[],1),
		  [{[0],[0]}]).



