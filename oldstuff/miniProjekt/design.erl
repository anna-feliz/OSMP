addPartialSum ({Ap, Bp}, bas, PIDnext, PIDresult)
sidoeffekter:
skickar:
result & [carryOutList] -> PIDreslut
carryout -> PIDnext
return ok.

collect (numberOfListsToWaitFor, results, carryout)
numberOfListsToWaitFor: int
results: list
carryout: list
return {finishedResultList, finishedCarryOutList}

start (A, B, Base, Options)
spawnar addPartialSum
anropar print(A, B, collect)
return ok.

print (A, B, {result, carryout})
printa ut A, B, resultat och carryout
return ok.

splitList (A, B, N)
jämna till A och B m h a nollor
return [{Ap0, Bp0}, {Ap1, Bp1}, ..., {Ap(n-1), Bp(n-1)}]




obs
[most sig,    ,     ,      ,      ,      ,     ,least sig.]
    
     
