:- use_module(library(lists)).
:- use_module(library(clpfd)).
:- use_module(library(random)).


puzzle0([[0,0],[0,3],[2,0],[3,0],[2,1],[3,1],[2,2],[3,3]]).
% Solution
% [0,0] - [0,3]
% [2,0] - [3,1]
% [3,0] - [2,1]
% [2,2] - [3,3]

% [1, 2, 3, 6, 4, 5, 7, 8] -> house indexes

connect(Houses, MaxX, MaxY):-
    
    /*
        let's assume ALL LISTS FLAT 
            -houses is a list of integers, each 2 integers is (X,Y) of an house
            -connections refer to houses using indexes of houses list
            -connections are not a list of lists, but a list of integers, and each 2 consecutive integers  are indexes to house and represent
            a connection
    */
    length(Houses, NHouses),
    length(Connections, NHouses),

    append(Houses, FlatHouses),

    domain(Connections, 1, NHouses),
    all_distinct(Connections),

    %2 different distances
    length(Distances, 2),
    all_distinct(Distances),
    MaxDis is MaxX * MaxX + MaxY * MaxY,
    domain(Distances, 1, MaxDis),

    % write(Connections), nl,
    restrictConnectDistances(Connections, FlatHouses, Distances),

    append(Distances, Connections, Vars),
    labeling([], Vars),
    write('---Make the following connections---'), nl,
    printSolution(Houses, Connections),
    write('---End---'), nl.


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


computeDistance([X1, Y1], [X2, Y2], Dis):-
    DiffX #= X2 - X1,
    DiffY #= Y2 - Y1,
    Dis #= DiffX * DiffX + DiffY * DiffY.


printSolution(_, []).
printSolution(Houses, [I1, I2|Connections]):-
    nth1(I1, Houses, House1),
    nth1(I2, Houses, House2),
    computeDistance(House1, House2, Distance),
    write('Connection: '), write(House1), write(' <-> '), write(House2), write(' by a distance of '), write(Distance), nl,
    printSolution(Houses, Connections).



%generator ---------------------------------------------------
generate(NHouses, _, _):- NHouses mod 2 =\= 0, write('INVALID NUMBER OF HOUSES: must be an EVEN number').
generate(_, MaxX, _):- MaxX < 0, write('INVALID X DOMAIN RESTRICTION: must be >= 0').
generate(_, _, MaxY):- MaxY < 0, write('INVALID Y DOMAIN RESTRICTION: must be >= 0').

% generate(NHouses, MaxX, MaxY):-
%     MaxDis is MaxX * MaxX + MaxY * MaxY,
%     getRandomDiffNumb(D1, D2, 1, MaxDis),

% getRandomDiffNumb(N1, N2, LB, UB):-
%     random(LB, UB, N1), %interval [Lower, Upper[
%     random(LB, UB, N2),
%     N1 \= N2.

% getRandomDiffNumb(N1, N2, LB, UB):- getRandomDiffNumb(N1, N2, LB, UB).
    

generate(NHouses, MaxX, MaxY):-    
    /*
        let's assume ALL LISTS FLAT 
            -houses is a list of integers, each 2 integers is (X,Y) of an house
            -connections refer to houses using indexes of houses list
            -connections are not a list of lists, but a list of integers, and each 2 consecutive integers  are indexes to house and represent
            a connection
    */
    
    length(XCoords, NHouses), domain(XCoords, 0, MaxX),
    length(YCoords, NHouses), domain(YCoords, 0, MaxY),

    ensureDifferentHouseCoords(XCoords, YCoords),

    append(XCoords, YCoords, Coords),
    labeling([], Coords),
    write(XCoords), nl, write(YCoords).


ensureDifferent(_, [], []).
ensureDifferent([X1, Y1], [X2|XCoords], [Y2|YCoords]):-
    X1 #\= X2 #\/ Y1 #\= Y2,
    ensureDifferent([X1, Y1], XCoords, YCoords).

ensureDifferentHouseCoords([], []).
ensureDifferentHouseCoords([X|XCoords], [Y|YCoords]):-
    ensureDifferent([X, Y], XCoords, YCoords),
    ensureDifferentHouseCoords(XCoords, YCoords).
    