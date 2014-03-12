%@doc 
start (A, B, Base) ->
    start (A, B, Base, [free]).


%doc
%option: speculative, {sleep, Min, Max}, {numberOfLists, N} 
-spec (A, B, Base, Option) -> ok. when 
      A::list(),
      B::list(),
      Base::integer(),
      Option::list(atom() | tuple()). 





start (A, B, Base, Options) ->
    case isIn(numberOfLists, Options) of 
	false ->
	    N = length(A) div 5,
	    divideListsAndStart(A, B, Base, N);
	Place ->
	    {_numberOfLists, N} = lists:nth(Place, Options),
	    divideListsAndStart(A, B, Base, N);
	end


divideListsAndStart (A, B, Base, N) ->
    Lists = splitList(A, B, N),
    CollectPID = spawn(fun() -> collect(N, [], [])),
            spawn_actors (Lists, Base, CollectPID).




spawn_actors ([H|T], Base, CollectPID) ->
    spawn_link(fun() -> spawn_actors (T, Base) end),
    receive
	{'EXIT', _PID, CarryIn} ->
	    {ResultList, CarryOutList, PartialCarryOut} = addPartialSum (H, Base, CarryIn),
	    CollectPID ! {ResultList, CarryOutList},
	    exit{PartialCarryOut}
    end.




isIn(Elem, List) ->
    isIn(Elem, List, 1).

isIn(Elem, [], N) -> 
    false;
isIn(Elem, [H|T], N) ->
    case (element(1, H) == Elem) of
	true ->
	    N;
	false ->
	    isIn(Elem, T, N+1)
    end.





	


	    
