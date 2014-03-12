
-module(start).

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
start (A, B, Base, Options) ->
    case isIn(numberOfLists, Options) of 
	false ->
	    N = length(A) div 5,
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
    Lists = splitList(A, B, N),
    CollectPID = spawn(fun() -> collect(N, [], []) end),
    spawn_actors (Lists, Base, CollectPID, Speculative),
    ok.



%
-spec spawn_actors(List, Base, CollectPID, Speculative) -> ok when
      List::list(),
      Base::integer(),
      CollectPID::pid(),
      Speculative:: true | false.

spawn_actors ([H|T], Base, CollectPID, Speculative) ->
    spawn_link(fun() -> spawn_actors (T, Base, CollectPID, Speculative) end),
    case Speculative of 
	true ->
	    {With_ResultList, With_CarryOutList, With_PartialCarryOut} = addPartialSum (H, Base, 1),
	    {Without_ResultList, Without_CarryOutList, Without_PartialCarryOut} = addPartialSum (H, Base, 0),
	    receive
		{'EXIT', _PID, 1} ->
		    CollectPID ! {With_ResultList, With_CarryOutList},
		    exit(With_PartialCarryOut);
		{'EXIT', _PID, 0} ->
		    CollectPID ! {With_ResultList, With_CarryOutList},
		    exit(With_PartialCarryOut)
	    end;
	false ->
	    receive
		{'EXIT', _PID, CarryIn} ->
		    {ResultList, CarryOutList, PartialCarryOut} = addPartialSum (H, Base, CarryIn),
		    CollectPID ! {ResultList, CarryOutList},
		    exit(PartialCarryOut)
	    end
    end,
    ok.

%doc
-spec isIn(Elem, List) -> integer() | false when
      Elem::speculative | sleep | numberOfLists,
      List::list().

%doc returns true if Elem is the first element in any tuple in List
isIn(Elem, List) ->
    isIn(Elem, List, 1).

-spec isIn(Elem, List, N) -> integer() | false when
      Elem::speculative | sleep | numberOfLists,
      List::list(),
      N::integer().

isIn(Elem, [], N) -> 
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





	


	    
