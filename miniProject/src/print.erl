%% @author Fredrik Larsson
-module(print).
-export([print/5]).
%% @doc Prints the addition of two integers and the possible
%% carry over for each single digit addition.
%% 
%% 
%%
%% === Example ===
%% <div class ="example">```
%% > add:print([1, 8, 7], [3, 5], [2, 2, 2], [0, 1, 1]).
%%   11 
%%   -- 
%%   187
%%    35
%% + ---
%%   222
%%   ok'''
%% </div>
-spec print(A, B, Result, Carry, Base) -> ok when
      A :: list(),
      B :: list(),
      Result :: list(),
      Carry :: list(),
      Base :: integer().

print(A, B, Result, Carry, Base) ->
    %% Line 1:
    printSpace(),
    io:format("Addition done in base: ~p", [Base]),
    printNewLine(),

    %% Line 2:
    printSpace(),
    printL1(Carry),

    %% Line 3:
    printSpace(),
    printL2(Carry),

    %% Line 4:
    printSpace(),
    printSpace(),
    printNum(A),

    %% Line 5:
    printSpace(),
    printSpace(),
    printNum(B),
    
    %% Line 6:
    io:format("+"),
    if length(Result) > length(A) ->
	    do_nothing;
       length(Result) =< length(A) ->
	    printSpace()
    end,
    io:format(repeat($-, length(Result))),
    printNewLine(),

    %% Line 7:
    if length(Result) > length(A) ->
	    do_nothing;
       length(Result) =< length(A) ->
	    printSpace()
    end,
    printSpace(),
    printNum(Result).


printSpace() ->
    io:format(" ").


printNewLine() ->
    io:format("~n").


printL1([])->
    io:format("~n");
printL1([H|T]) ->
    case H == 1 of
	false ->
	    io:format(" "),
	    printL1(T);
	true ->
	    io:format("1"),
	    printL1(T)
    end.


printL2([]) ->
    io:format("~n");
printL2([H|T]) ->
    case H == 1 of
	false ->
	    io:format(" "),
	    printL2(T);
	true ->
	    io:format("-"),
	    printL2(T)
    end. 

printNum(L) ->
    printNum(L, 0).
printNum([],_) ->
    io:format("~n");
printNum([H|T], X) when H == 0, X == 0 ->
    io:format(" "),
    printNum(T, 0);
printNum([H|T], _) ->
    io:format(intToChar(H)),
    printNum(T, 1).


repeat(Char, N) ->
    [Char || _ <- lists:seq(1,N)].


intToChar(Int) when Int >= 0, Int =< 9 ->
    [Int+48];
intToChar(Int) when Int >= 10, Int =< 36 -> %% ???
    [Int+55];
intToChar(_) ->
    erlang:error(error,nAN).
 
