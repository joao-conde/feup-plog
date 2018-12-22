restrictConnectDistances([], _, _).
restrictConnectDistances([I1, I2|Connections], FlatHouses, Distances):-
    HX1 #= I1 * 2 - 1,
    HY1 #= I1 * 2,
    HX2 #= I2 * 2 - 1,
    HY2 #= I2 * 2,
    element(HX1, FlatHouses, X1), 
    element(HY1, FlatHouses, Y1), 
    element(HX2, FlatHouses, X2), 
    element(HY2, FlatHouses, Y2),
    computeDistance([X1, Y1], [X2, Y2], Dis),
    element(_, Distances, Dis),
    restrictConnectDistances(Connections, FlatHouses, Distances).


ensureUnique(_, []).
ensureUnique([X1, Y1], [X2, Y2|Houses]):-
    X1 #\= X2 #\/ Y1 #\= Y2,
    ensureUnique([X1, Y1], Houses).

ensureDifferentHouses([]).
ensureDifferentHouses([X1, Y1|FlatHouses]):-
    ensureUnique([X1, Y1], FlatHouses),
    ensureDifferentHouses(FlatHouses).
    

buildHouseList([], Houses, Houses).
buildHouseList([X, Y|FlatHouses], Houses, Acc):-
    buildHouseList(FlatHouses, Houses, [[X, Y]|Acc]).