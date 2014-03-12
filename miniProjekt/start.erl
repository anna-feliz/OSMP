-module(start).
-export([start/3, start/4]).

%@doc 
start (A, B, Base) ->
    start (A, B, Base, []).


%doc returns ok. 
% A, B are lists with 1 digit integers to be added together, Base is in which number base A and B are represented, Options is the options that can be chosen.   
%option: speculative, {sleep, Min, Max}, {numberOfLists, N} 
-spec start(A, B, Base, Options) -> ok when 
      A::list(),
      B::list(),
      Base::integer(),
      Options::list().


start (AInt, BInt, Base, Options) ->
    A = intToList(AInt, Base),    
    B = intToList(BInt, Base),
    case isIn(numberOfLists, Options) of 
	false ->
	    N = length(A),
	    divideListsAndStart(A, B, Base, N, isIn(speculative, Options));
	Place ->
	    {_numberOfLists, N} = lists:nth(Place, Options),
	    divideListsAndStart(A, B, Base, N, isIn(speculative, Options))
    end,
    
    ok.

%startar en ny process med PID CollectPID, som arbetar med att samla ihop all data.
-spec divideListsAndStart(A, B, Base, N, Speculative) -> ok when
      A::list(),
      B::list(),
      Base::integer(),
      N::integer(),
      Speculative::true | false.

divideListsAndStart (A, B, Base, N, Speculative) ->
    io:format("A: ~p\nB: ~p\nN: ~p\n", [A, B, N]),
    Lists = splitLists(A, B, N),
    CollectPID = spawn(fun() -> collect(A, B, N, [], []) end),
    spawn_actors (Lists, Base, CollectPID, Speculative, self()),
    ok.



%
-spec spawn_actors(List, Base, CollectPID, Speculative, ParentPID) -> ok when
      List::list(),
      Base::integer(),
      CollectPID::pid(),
      Speculative:: true | false,
      ParentPID::pid().
spawn_actors ([], _Base, _CollectPID, _Speculative, ParentPID) ->
    ParentPID ! {carryOut, 0};
spawn_actors ([H|T], Base, CollectPID, Speculative, ParentPID) ->
    spawn_link(fun() -> spawn_actors (T, Base, CollectPID, Speculative, self()) end),
    case Speculative of 
	true ->
	    {With_ResultList, With_CarryOutList, With_PartialCarryOut} = addPartialSum:addPartialSum (H, Base, 1),
	    {Without_ResultList, Without_CarryOutList, Without_PartialCarryOut} = addPartialSum:addPartialSum (H, Base, 0),
	    receive
		{carry, 1} ->
		    CollectPID ! {With_ResultList, With_CarryOutList},
		    ParentPID ! {carryOut, With_PartialCarryOut};
		{carry, 0} ->
		    CollectPID ! {Without_ResultList, Without_CarryOutList},
		    ParentPID ! {carryOut, Without_PartialCarryOut}
	    end;
	false ->
	    receive
		{carry, CarryIn} ->
		    io:format("hejsan", []),

		    {ResultList, CarryOutList, PartialCarryOut} = addPartialSum:addPartialSum (H, Base, CarryIn),
		    CollectPID ! {ResultList, CarryOutList},
		    ParentPID ! {carryOut, PartialCarryOut}
	    end
    end,
    ok.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                        help functions                                %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%doc returns false if Elem is not the first element of any tuple in List
%else return the position of that tuple where 1 is the first position in List.
-spec isIn(Elem, List) -> integer() | false when
      Elem::speculative | sleep | numberOfLists,
      List::list().


isIn(Elem, List) ->
    isIn(Elem, List, 1).

-spec isIn(Elem, List, N) -> integer() | false when
      Elem::speculative | sleep | numberOfLists,
      List::list(),
      N::integer().

isIn(_Elem, [], _N) -> 
    false;
isIn(Elem, [H|T], N) ->
    case Elem of
	speculative ->
	    case H = Elem of
		true ->
		    true;
		false ->
		    isIn(Elem, T, N+1)
	    end;
	true ->
	    case (element(1, H) == Elem) of
		true ->
		    N;
		false ->
		    isIn(Elem, T, N+1)
	    end
    end.


-spec intToList(N, Base) -> list() when
      N::integer(),
      Base::integer().

intToList(N, Base) ->
    intToList(N, Base, []).


-spec intToList(N, Base, List) -> list() when
      N::integer(),
      Base::integer(),
      List::list().

intToList(0, _Base, List) ->
    List;
intToList(N, Base, List) ->
    intToList(N div Base, Base, [(N rem Base)|List]).




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                collect/5                                %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



%% @doc Receives Results and Carryout, appends it with new lists FinalResults and FinalCarryOut and returns it 
-spec collect(A, B, NumberOfListsToWaitFor, Results, CarryOut) -> {list(), list()} when 
      A::list(),
      B::list(),
      NumberOfListsToWaitFor::integer(), 
      Results::[integer()], 
      CarryOut::[integer()]. 

collect(A, B, N, FinalResults, FinalCarryOut) when N < numberOfListsToWaitFor -> 
    receive {Results, CarryOut} -> 
	    collect(A, B, N, lists:append([Results, FinalResults]), lists:append([CarryOut, FinalCarryOut])) 
    end; 

collect(A, B, _NumberOfListsToWaitfor, Results, CarryOut) -> 
    io:format("~p \n~p \n~p\n ~p\n", [A, B, Results, CarryOut]).
















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
      List::list(),
      Length::integer(),
      ListList::list(),
      N::integer().

splitLists(List, _Length, ListList, 1) ->
    lists:reverse([List | ListList]);
splitLists(List, Length, ListList, N) ->
    io:format("~p \n", [Length]),
    {First, Second} = lists:split (Length, List),
    splitLists(Second, Length, [First | ListList], N-1).







-spec addZeros(N, List) -> list() when
      N::integer(),
      List::list().

addZeros(N, List) when length(List) == N ->  
    List;
addZeros(N, List) ->
    addZeros(N, [0|List]).


