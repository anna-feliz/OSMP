


%% @doc Receives Results and Carryout, appends it with new lists FinalResults and FinalCarryOut and returns it 
-spec collect(NumberOfListsToWaitFor, Results, CarryOut) -> {list(), list()} when 
      NumberofListToWaitFor::integer(), 
      Results::[integer()], 
      Carryout::[integer()]. 

collect(N, FinalResults, FinalCarryOut) when N < numberOfListsToWaitFor -> 
    receive {Results,CarryOut} -> 
	    collect(N, lists:append([Results,FinalResults]), lists:append([CarryOut,FinalCarryOut])) 
    end; 

collect(_NumberOfListsToWaitfor, Results, CarryOut) -> {finalResults,finalCarryout}.
