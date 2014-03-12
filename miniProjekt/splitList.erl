%% @author Felix Färsjö, Jimmy Holm, Fredrik Larsson, Anna Nilsson, Philip Åkerfeldt

-module(splitList).



%% To use EUnit we must include this library.
-include_lib("eunit/include/eunit.hrl").

%% TODO:
%% använd split(N, List1) -> list2 (N första elementen) + list3 (tail) 
%% anropa split två gånger

%% @doc Will split the lists A and B in N equal long parts.
-spec splitList() -> [{}] when 
      
splitList(A, B, N) -> 
    %% Kolla om listan A är mindre än N
    %% om nej --> anropa lists:split(N, A) 
    

















%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                         EUnit Test Cases                                  %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% EUnit adds the splitlist() function to this module. 

start_test() ->
    ?assertMatch(true, is_pid(start(10))).
