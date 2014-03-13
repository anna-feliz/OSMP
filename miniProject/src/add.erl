%% @doc Erlang mini project.
-module(add).
-export([start/3, start/4]).
-import(addPartialSum, [addPartialSum/3, makeString/1]).
-import(print, [print/5]).
-import(splitLists, [splitLists/3, splitLists/4]).

%% To use EUnit we must include this:
-include_lib("eunit/include/eunit.hrl").

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
	    N = 1,
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
    %io:format("A: ~p\nB: ~p\nN: ~p\n", [A, B, N]),
    Lists = splitLists(A, B, N),
    %io:format("~p\n", [Lists]),
    CollectPID = spawn(fun() -> collect(A, B, N, [], []) end),
    MyPID = self(),
    spawn(fun() -> spawn_actors (Lists, Base, CollectPID, Speculative, MyPID, MyPID) end),
    ok.



%
-spec spawn_actors(List, Base, CollectPID, Speculative, ParentPID, SuperParentPID) -> ok when
      List::list(),
      Base::integer(),
      CollectPID::pid(),
      Speculative:: true | false,
      ParentPID::pid(),
      SuperParentPID::pid().
spawn_actors ([], _Base, CollectPID, _Speculative, ParentPID, SuperParentPID) ->
    %io:format("last link\n", []),

    case SuperParentPID == ParentPID of
	false ->
	    ParentPID ! {carry, 0};
	true ->
	    CollectPID ! {carry, 0}
    end,
    ok;
spawn_actors ([H|T], Base, CollectPID, Speculative, ParentPID, SuperParentPID) ->
    MyPID = self(),
    spawn(fun() -> spawn_actors (T, Base, CollectPID, Speculative, MyPID, SuperParentPID) end),
    case Speculative of 
	true ->
	    {With_ResultList, With_CarryOutList, With_PartialCarryOut} = addPartialSum:addPartialSum (H, Base, 1),
	    {Without_ResultList, Without_CarryOutList, Without_PartialCarryOut} = addPartialSum:addPartialSum (H, Base, 0),
	    receive
		{carry, 1} ->
		    CollectPID ! {With_ResultList, With_CarryOutList},
		    case SuperParentPID == ParentPID of
			false ->
			    ParentPID ! {carry, With_PartialCarryOut};	    
			true ->
			    CollectPID ! {carry, With_PartialCarryOut}
		    end;

		{carry, 0} ->
		    CollectPID ! {Without_ResultList, Without_CarryOutList},
		    case SuperParentPID == ParentPID of
			false ->
			    ParentPID ! {carry, Without_PartialCarryOut};
			true ->
			    %io:format("I am here"),
			    CollectPID ! {carry, Without_PartialCarryOut}
		    end
		end;
	false ->
	    %io:format("\nBefore receive. H: ~p, T: ~p\n", [H, T]),
	    receive
		{carry, CarryIn} ->
		    %io:format("hejsan\n", []),

		    {ResultList, CarryOutList, PartialCarryOut} = addPartialSum:addPartialSum (H, Base, CarryIn),
		    CollectPID ! {ResultList, CarryOutList},
		    case SuperParentPID == ParentPID of
			false ->
			    ParentPID ! {carry, PartialCarryOut};
			true ->
			    CollectPID ! {carry, PartialCarryOut}
		    end;
		_ ->
		    io:format("Du är cool", [])
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
					case speculative == H of
		true -> 
						case H == Elem of
			true ->
							true;
			false ->
							isIn(Elem, T, N+1)
						end;
		false ->
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

	collect(A, B, N, FinalResults, FinalCarryOut) ->
					collect(A, B, N, FinalResults, FinalCarryOut, 0).

	-spec collect(A, B, NumberOfListsToWaitFor, Results, CarryOut, Count) -> {list(), list()} when 
							A::list(),
							B::list(),
							NumberOfListsToWaitFor::integer(), 
							Results::[integer()], 
							CarryOut::[integer()],
							Count::integer().

	collect(A, B, N, FinalResults, FinalCarryOut, Count) when N >= Count ->
					io:format("A: ~p~nB: ~p\nFinalResults: ~p~nFinalCarryOut: ~p~n", [A, B, FinalResults, FinalCarryOut]),
					receive 
		{carry, CarryOut} -> 
						collect(A, B, N, FinalResults, [CarryOut|FinalCarryOut], Count); 
		{Results, CarryOut} -> 
	    collect(A, B, N, lists:append([Results, FinalResults]), lists:append([CarryOut, FinalCarryOut]), Count+1)
    end; 
collect(A, B, _NumberOfListsToWaitfor, Results, CarryOut, _Count) -> 
				io:format("A: ~p ~nB: ~p \nResults: ~p~nCarryOut: ~p~n", [A, B, Results, CarryOut]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%            Tests                       %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

intToList_test_() -> 
    [?_assertEqual([], intToList(0, 10)),
     ?_assertEqual([1, 2, 3, 4], intToList(1234, 10))].
