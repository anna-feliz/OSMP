%% @author Grupp 4 
-module(addPartialSum).
-export([addPartialSum/3]).

-include_lib("eunit/include/eunit.hrl").

%% @doc Add two partial sums and return A tuple of type {List, List, Number} on success.
%% The contents of the returned tuple is: The <i>partial sum-list</i>, the <i>Carry-Out list</i> 
%% and the addition's <i>final carry-out value</i>.

-spec addPartialSum({A, B}, Base, CarryIn) -> ok when
						A::list(),
						B::list(),
						Base::number(),
						CarryIn::number().
addPartialSum({A, B}, Base, CarryIn) ->
				addRecursive({lists:reverse(A), lists:reverse(B)}, Base, [], [CarryIn]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                  Helper Functions                   %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec addTwo(A, B, C, Base) -> number() when A::number(),
																																													B::number(),
																																													C::number(),
																																													Base::number().

addTwo(A, B, C, Base) when (A+B+C) >= Base ->
				(A+B+C)-Base;
addTwo(A, B, C, Base) when (A+B+C) <  Base ->
				(A+B+C).

-spec carryTwo(A, B, C, Base) -> number() when A::number(),
																																															B::number(),
																																															C::number(),
																																															Base::number().
carryTwo(A, B, C, Base) when (A+B+C) >= Base ->
				1;
carryTwo(A, B, C, Base) when (A+B+C) <  Base ->
				0.


-spec addRecursive({A, B}, Base, Result, CarryOut) -> {list(), list(), number()} when
						A::list(),
						B::list(),
						Base::number(),
						Result::list(),
						CarryOut::list().
addRecursive({[], []}, _, Result, [C|CarryOut])->
				{Result, CarryOut, C};
addRecursive({[Ah|At], [Bh|Bt]}, Base, Result, [C|CarryOut]) ->
				addRecursive({At, Bt}, Base, [addTwo(Ah, Bh, C,Base)|Result], [carryTwo(Ah, Bh, C, Base)|[C|CarryOut]]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                     EUnit Tests                     %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
add_Recursive_test_() ->
				[
					?_assertEqual(addRecursive({[4,3,2,0],[5,7,3,1]}, 10, [],[0]), {[1,6,0,9], [0,1,0,0], 0}),
				 ?_assertEqual(addRecursive({[9,0,9]  ,[1,0,1]}, 10, [],[0]), {[0,1,0], [0,1,0], 1}),
				 ?_assertEqual(addRecursive({[9,0],[0,1]}, 10, [], [1]), {[2,0],[1,1],0}),
					?_assertEqual(addRecursive({[15],[1]}, 16, [], [0]), {[0],[0],1}),
					?_assertEqual(addRecursive({[1],[1]}, 2, [], [0]), {[0],[0],1}),
					?_assertEqual(addRecursive({[4],[0]}, 5, [], [1]), {[0],[1],1}),
					?_assertEqual(addPartialSum({[0,2,3,4],[1,3,7,5]}, 10, 0), {[1,6,0,9], [0,1,0,0], 0}),
				 ?_assertEqual(addPartialSum({[9,0,9]  ,[1,0,1]}, 10, 0), {[0,1,0], [0,1,0], 1}),
				 ?_assertEqual(addPartialSum({[0,9],[1,0]}, 10,1), {[2,0],[1,1],0}),
					?_assertEqual(addPartialSum({[15],[1]}, 16, 0), {[0],[0],1}),
					?_assertEqual(addPartialSum({[1],[1]}, 2, 0), {[0],[0],1}),
					?_assertEqual(addPartialSum({[4],[0]}, 5, 1), {[0],[1],1})
				].

add_AddTwo_test_() ->
				[
					?_assertEqual(addTwo(1,  1,  0, 10), 2),
					?_assertEqual(addTwo(1,  1,  0,  2), 0),
					?_assertEqual(addTwo(15, 1,  0, 16), 0),
					?_assertEqual(addTwo(4,  2,  0, 10), 6),
					?_assertEqual(addTwo(1,  0,  1,  2), 0)
				].

add_CarryTwo_test_() ->
				[
					?_assertEqual(carryTwo(1,  1, 0, 10), 0),
					?_assertEqual(carryTwo(1,  1, 0,  2), 1),
					?_assertEqual(carryTwo(15, 1, 0, 16), 1),
					?_assertEqual(carryTwo(4,  2, 0, 10), 0),
					?_assertEqual(carryTwo(1,  0, 1,  2), 1)
				].
