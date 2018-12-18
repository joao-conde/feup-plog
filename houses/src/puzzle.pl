%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%      Utility Predicates Module      %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

connect(Houses):-
    %There are only 2 distances and they're different
    length(Distances, 2),
    all_distinct(Distances),

    %One connection per 2 houses (pairs) means half the number of houses as connections
    length(Houses, NHouses),
    NConnections #= NHouses // 2,
    length(Connections, NConnections),
    
    restrictDistances(Connections, Distances), %restrict every connection to have one of the two distances
    
    
    %All the houses must be connected
    append(Connections, ConnectedHouses),
    ensureAllConnected(Houses, ConnectedHouses), %table

    removeSymmetries(Connections), %avoid symmetries

    %flatten list and labeling
    append(ConnectedHouses, HousesCoordinates),
    labeling([], HousesCoordinates),
    write(Connections).

/*
    All distances of all connections are one of the two distances
    Distance is kept squared to keep it an integer i.e. dist(connection) = dist([[x1, y1], [x2, y2]]) = (x2-x1)^2 + (y2-y1)^2
*/
restrictDistances([], _).
restrictDistances([[[X1, Y1], [X2, Y2]]|Connections], Distances):-
    DiffX #= X2 - X1,
    DiffY #= Y2 - Y1,
    Dis #= DiffX * DiffX + DiffY * DiffY,
    member(Dis, Distances), %element
    restrictDistances(Connections, Distances).

/*
    Ensures all houses are connected
*/
ensureAllConnected([], _).
ensureAllConnected([H|Houses], ConnectedHouses):-
    member(H, ConnectedHouses),
    ensureAllConnected(Houses, ConnectedHouses).

/*
    Remove symmetries and connection permutations in final result
*/
removeSymmetries([_]).
removeSymmetries([[[X1, _], [X2, _]], [[X3, Y3], [X4, Y4]]|Connections]):-
    X1 #=< X2,
    X1 #=< X3,
    X3 #=< X4,
    removeSymmetries([[[X3, Y3], [X4, Y4]]|Connections]).