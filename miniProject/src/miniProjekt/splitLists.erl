-module(splitLists).
-export([splitLists/3, splitLists/4]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                              splitLists/3                               %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


-spec splitLists(A, B, N) -> list() when
      A::list(),
      B::list(),
      N::integer().
    
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
    LengthOfSublists = length(NewA) div N,
    ALists = splitLists(NewA, LengthOfSublists, [], N),
    BLists = splitLists(NewB, LengthOfSublists, [], N),
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


