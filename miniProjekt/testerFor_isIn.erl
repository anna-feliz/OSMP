%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                         EUnit Test Cases                                  %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% EUnit adds the fifo:test() function to this module. 

%% All functions with names ending wiht _test() or _test_() will be
%% called automatically by death:test()

isIn_test() ->

    ?assertMatch(1, isIn(speculative, [speculative, {numberOfLists, 4}])),
    ?assertMatch(2, isIn(numberOfLists, [speculative, {numberOfLists, 4}])),
    ?assertMatch(false, isIn(speculative, [{numberOfLists, 4}])).
    
    %%?assertMatch(false, isIn(speculative, [speculative, {sleep, min, max}, {numberOfLists, 4}])).
