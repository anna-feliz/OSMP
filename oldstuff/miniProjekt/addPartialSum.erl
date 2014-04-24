-module(addPartialSum).
-export([addPartialSum/3, makeString/1]).

-include_lib("eunit/include/eunit.hrl").

-spec makeString(A) -> string() when A::number().
makeString(15) ->
				"F";
makeString(14) ->
				"E";
makeString(13) ->
				"D";
makeString(12) ->
				"C";
makeString(11) ->
				"B";
makeString(10) ->
				"A";
makeString(9) ->
				"9";
makeString(8) ->
				"8";
makeString(7) ->
				"7";
makeString(6) ->
				"6";
makeString(5) ->
				"5";
makeString(4) ->
				"4";
makeString(3) ->
				"3";
makeString(2) ->
				"2";
makeString(1) ->
				"1";
makeString(0) ->
				"0";
makeString(_) ->
				erlang:error(error,nAN).

-spec makeNumber(A, Base) -> number() when A::number(),
																																											Base::number().
makeNumber($f, Base) when Base > 15 ->
				15;
makeNumber($F, Base) when Base > 15 ->
				15;
makeNumber($e, Base) when Base > 14 ->
				14;
makeNumber($E, Base) when Base > 14 ->
				14;
makeNumber($d, Base) when Base > 13 ->
				13;
makeNumber($D, Base) when Base > 13 ->
				13;
makeNumber($c, Base) when Base > 12 ->
				12;
makeNumber($C, Base) when Base > 12 ->
				12;
makeNumber($b, Base) when Base > 11 ->
				11;
makeNumber($B, Base) when Base > 11 ->
				11;
makeNumber($a, Base) when Base > 10 ->
				10;
makeNumber($A, Base) when Base > 10 ->
				10;
makeNumber(9, Base) when Base > 9 ->
				9;
makeNumber(8, Base) when Base > 8->
				8;
makeNumber(7, Base) when Base > 7 ->
				7;
makeNumber(6, Base) when Base > 6 ->
				6;
makeNumber(5, Base) when Base > 5 ->
				5;
makeNumber(4, Base) when Base > 4 ->
				4;
makeNumber(3, Base) when Base > 3 ->
				3;
makeNumber(2, Base) when Base > 2 ->
				2;
makeNumber(1, Base) when Base > 1 ->
				1;
makeNumber(0, _) ->
				0;
makeNumber(_,_) ->
				erlang:error(error, nAN).


-spec addTwo(A, B, C, Base) -> number() when A::number(),
																																													B::number(),
																																													C::number(),
																																													Base::number().

addTwo(A, B, C, Base) when A+B+C >= Base->
				(A+B+C)-Base;
addTwo(A, B, C, Base) ->
				(A+B+C).

-spec carryTwo(A, B, C, Base) -> number() when A::number(),
																																															B::number(),
																																															C::number(),
																																															Base::number().
carryTwo(A, B, C, Base) when (A+B+C) >= Base ->
				1;
carryTwo(A, B, C, Base) ->
				0.


-spec addRecursive({A, B}, Base, Result, CarryOut) -> {list(), list(), number()} when
						A::list(),
						B::list(),
						Base::number(),
						Result::list(),
						CarryOut::list().
addRecursive({[], []}, Base, Result, [C|CarryOut])->
				{Result, CarryOut, C};
addRecursive({[Ah|At], [Bh|Bt]}, Base, Result, [C|CarryOut]) ->
				addRecursive({At, Bt}, Base, [addTwo(makeNumber(Ah,Base), makeNumber(Bh,Base), C,Base)|Result], [carryTwo(Ah, Bh, C, Base)|[C|CarryOut]]).

%@doc Add two partial sums and return A tuple of type {List, List, Numbe} on success.
-spec addPartialSum({A, B}, Base, CarryIn) -> ok when
						A::list(),
						B::list(),
						Base::number(),
						CarryIn::number().
addPartialSum({A, B}, Base, CarryIn) ->
				addRecursive({lists:reverse(A), lists:reverse(B)}, Base, [], [CarryIn]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                     EUnit Tests                     %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
add_Recursive_test_() ->
				[
					?_assertEqual(addRecursive({[4,3,2,0],[5,7,3,1]}, 10, [],[0]), {[1,6,0,9], [0,1,0,0], 0}),
				 ?_assertEqual(addRecursive({[9,0,9]  ,[1,0,1]}, 10, [],[0]), {[0,1,0], [0,1,0], 1}),
				 ?_assertEqual(addRecursive({[9,0],[0,1]}, 10, [], [1]), {[2,0],[1,1],0}),
					?_assertEqual(addRecursive({[$F],[1]}, 16, [], [0]), {[0],[0],1}),
					?_assertEqual(addRecursive({[1],[1]}, 2, [], [0]), {[0],[0],1}),
					?_assertEqual(addRecursive({[4],[0]}, 5, [], [1]), {[0],[1],1}),
					?_assertEqual(addPartialSum({[0,2,3,4],[1,3,7,5]}, 10, 0), {[1,6,0,9], [0,1,0,0], 0}),
				 ?_assertEqual(addPartialSum({[9,0,9]  ,[1,0,1]}, 10, 0), {[0,1,0], [0,1,0], 1}),
				 ?_assertEqual(addPartialSum({[0,9],[1,0]}, 10,1), {[2,0],[1,1],0}),
					?_assertEqual(addPartialSum({[$F],[1]}, 16, 0), {[0],[0],1}),
					?_assertEqual(addPartialSum({[1],[1]}, 2, 0), {[0],[0],1}),
					?_assertEqual(addPartialSum({[4],[0]}, 5, 1), {[0],[1],1})
				].


