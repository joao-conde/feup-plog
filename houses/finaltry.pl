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
generate(NHouses, _, _):- NHouses mod 2 =\= 0, write('INVALID RESTRICTION: number of houses must be an even number').
generate(_, MaxX, _):- MaxX =< 1, write('INVALID RESTRICTION: X domain upper bound must be > 1').
generate(_, _, MaxY):- MaxY =< 1, write('INVALID RESTRICTION: Y domain upper bound must be > 1').

/*
    1 - generate 2 random distances
    2 - generate N/2 pairs of houses at distance D1 or D2
*/
% generate(NHouses, MaxX, MaxY):-
%     MaxDis is MaxX * MaxX + MaxY * MaxY,
%     getRandomDiffNumb(D1, D2, 1, MaxDis),
%     generateHouses([D1, D2], NHouses, Houses, []),
%     write(Houses).

% getRandomDiffNumb(N1, N2, LB, UB):-
%     random(LB, UB, N1), %interval [Lower, Upper[
%     random(LB, UB, N2),
%     N1 \= N2.

% getRandomDiffNumb(N1, N2, LB, UB):- getRandomDiffNumb(N1, N2, LB, UB).



% generateHouses([D1, D2], 0, Houses, Houses).
% generateHouses([D1, D2], NHouses, Houses, Acc):-
%     NHouses1 is NHouses - 1,
%     random(0, 2, 0), %interval [Lower, Upper[
%     generateHousesPair(D1, HousesPair, []),
%     generateHouses([D1, D2], NHouses1, Houses, Acc).

% generateHouses([D1, D2], NHouses, Houses, Acc):-
%     NHouses1 is NHouses - 1,
%     random(0, 2, 1), %interval [Lower, Upper[
%     generateHousesPair(D2, HousesPair, []),
%     generateHouses([D1, D2], NHouses1, Houses, Acc).


% generateHousesPair(D, HousePair, Acc):-
%     random(),

% ensureDifferent(_, [], []).
% ensureDifferent([X1, Y1], [X2|XCoords], [Y2|YCoords]):-
%     X1 #\= X2 #\/ Y1 #\= Y2,
%     ensureDifferent([X1, Y1], XCoords, YCoords).

% ensureDifferentHouseCoords([], []).
% ensureDifferentHouseCoords([X|XCoords], [Y|YCoords]):-
%     ensureDifferent([X, Y], XCoords, YCoords),
%     ensureDifferentHouseCoords(XCoords, YCoords).


% buildFlatHouseList([], [], FlatHouses, FlatHouses).
% buildFlatHouseList([X|XCoords], [Y|YCoords], FlatHouses, Acc):-
%     append([X], [Y], Coords),
%     append(Coords, Acc, Acc2),
%     buildFlatHouseList(XCoords, YCoords, FlatHouses, Acc2). 
    