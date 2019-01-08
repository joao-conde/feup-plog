%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%            CLPFD           %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


/*  connect(+Houses, -Connections, -Runtime)
    
    Solver predicate. Takes a list of houses. Each house is a list of coordinates [x, y]. Coordinates X and Y go from [0, Range].
    Computes the solution and returns the connections made in Connections and the total runtime in Runtime.

    ---Implementation---
     
    We focused on working with FLAT lists of integers. It makes it easier to use some CLPFD predicates without
    constantly flattening the lists. As such below we describe our representation.

    1 - The argument passed onto the predicate is a list of lists, each representing an house as a pair [x, y]
    2 - That list is flattenned thus we have a FlatHouses list where each 2 consecutive integers are coordinates of an house.
    3 - Connections is also a flat list of integers. Each 2 integers represent a connection between the house located at the
    position of the first connection index and the house located at the position of the second.
    This means connections are a list of integers where each 2 consecutive integers are indexes to the Houses list and represent a connection.
    4 - Distances are kept squared so they are always integers (sum of 2 squared integers).
*/
connect(Houses, Connections, Runtime):-
    statistics(runtime,[Start|_]), %statistics - runtime calculation (ms) 

    append(Houses, FlatHouses), 
    max_member(Max, FlatHouses), %max coordinate will represent our domain range
    
    length(Houses, NHouses),
    length(Connections, NHouses), %each connection integer will point to an house
    domain(Connections, 1, NHouses), 
    all_distinct(Connections), %all the houses must be connected so all are different

    Distances = [D1, D2], %there are 2 different distances
    all_distinct(Distances),
    MaxDis is 2 * Max * Max, 
    domain(Distances, 1, MaxDis), %can't be 0 (2 houses same coords) and range from 1 to the maximum representing a diagonal on our "squared matrix" domain range

    restrictConnectDistances(Connections, FlatHouses, Distances), %connections will have one of the two existing distances

    %eliminate symmetries
    D1 #< D2,
    removeCoordsPermutation(Connections), %avoids permutations of type A <-> B and B <-> A
    removeConnectionsPermutation(Connections), %avoids permutations of type [A<->B, C<->D] and [C<->D, A<->B]
    labeling([ffc, down], [D1, D2|Connections]),

    %statistics - runtime calculation (ms) 
    statistics(runtime,[Stop|_]),
    Runtime is Stop - Start.


/*  generate(-Houses, +NHouses, +Domain)
    
    Generator predicate. Returns generated puzzle in Houses. Takes the even number of houses to generate and the X and Y upper bound limit.

    ---Implementation---
     
    We followed a similiar line of thought as previously. The representation is the same. The restrictions end up being the same except now
    we don't know the house list and we need to randomize the distances.

    1 - While most is the same, there is now the extra need of ensuring all pairs [x,y] are different. That means between 2 houses
    of the generated list, either their X is different or their Y is different (or both are different).

    2 - We also needed to not only restrict the distances to possible values but also to have different values for each generation. For that
    we followed: https://sicstus.sics.se/sicstus/docs/4.0.4/html/sicstus/Enumeration-Predicates.html
    Basically, we created our custom value selection method for the distance variables. We made it random.
*/
generate(Houses, NHouses, Domain):-
    NFlatHouses is NHouses * 2, 
    length(FlatHouses, NFlatHouses), %for each house we have 2 coordinates, so double-sized list
    domain(FlatHouses, 0, Domain), %the coordinates can go from 0 to a defined upper bound
    ensureDifferentHouses(FlatHouses), %all house coordinates are different

    length(Connections, NHouses), %same as before, connections are indexes to houses
    domain(Connections, 1, NHouses),
    all_distinct(Connections), %all indexes to all houses appear because all connected
    
    MaxDis is 2 * Domain * Domain,
    Distances = [D1, D2],
    domain(Distances, 1, MaxDis), %distances go from [1, MaxDis] where MaxDis represents the distance of a "diagonal"
    all_distinct(Distances),  %they're both different

    restrictConnectDistances(Connections, FlatHouses, Distances), %same as before connections have one of the two distances
    
    %symmetries removal
    removeCoordsPermutation(Connections),
    removeConnectionsPermutation(Connections),
    D1 #< D2,

    append(FlatHouses, Connections, Vars), %unique list of domain variables
    append(Vars, Distances, Vars2),
    labeling([value(randomLabeling), ffc], Vars2),
    buildHouseList(FlatHouses, Houses, []). %turn into puzzle input format


/*  restrictConnectDistances(+Connections, +FlatHouses, +Distances)
    
    Ensures all connections have one of two distances. 
    Takes the connections list, a list of integers where for each 2 consecutive integers there is a connection between
    the house pointed by the first integer and the house pointed by the second integer.
    Takes the FlatHouses list to be able to compute the distance of the connection and the Distances list to check it is contained there.
*/
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

/*  ensureUnique(+[X1, Y1], +Houses)
    
    Ensures the pair [X1, Y1] is unique in the list of Houses.
    A coordinate is unique if there is no other with the same X and Y.
*/
ensureUnique(_, []).
ensureUnique([X1, Y1], [X2, Y2|Houses]):-
    X1 #\= X2 #\/ Y1 #\= Y2,
    ensureUnique([X1, Y1], Houses).


/*  ensureDifferentHouses(+FlatHouses)
    
    Ensures all the houses are different from each other.
*/
ensureDifferentHouses([]).
ensureDifferentHouses([X1, Y1|FlatHouses]):-
    ensureUnique([X1, Y1], FlatHouses),
    ensureDifferentHouses(FlatHouses).
    

/*  buildHouseList(-FlatHouses, +Houses, -Acc)
    
    Builds a puzzle formatted list i.e. a list of houses (lists [x,y]) from the FlatHouses list.
*/
buildHouseList([], Houses, Houses).
buildHouseList([X, Y|FlatHouses], Houses, Acc):-
    buildHouseList(FlatHouses, Houses, [[X, Y]|Acc]).


/*  removeCoordsPermutation(+Connections)
    
    Ensures that connection A<->B appears once and not also B<->A.
*/
removeCoordsPermutation([]).
removeCoordsPermutation([C1, C2|Connections]):-
	C1 #=< C2,
	removeCoordsPermutation(Connections).


/*  removeConnectionsPermutation(+Connections)
    
    Ensures that connection list [A<->B, C<->D] appears once and not also permutations like [C<->D, A<->B].
*/
removeConnectionsPermutation([_, _]).
removeConnectionsPermutation([C1, _, C3|Connections]):-
	C1 #=< C3,
	removeConnectionsPermutation([C3|Connections]).
