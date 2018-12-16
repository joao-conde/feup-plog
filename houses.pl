:-use_module(library(lists)).
:-use_module(library(clpfd)).

% Solution
% [0,0] - [0,3]
% [2,0] - [3,1]
% [3,0] - [2,1]
% [2,2] - [3,3]
puzzle0([[0, 0], [0, 3], [2, 0], [3, 0], [2, 1], [3, 1], [2, 2], [3, 3]]).

%why does solve([[_, _], [_, _], [_, _], [_, _]]). not work DANIEL!=??!

solve(Houses):-
    %2 distances
    length(Distances, 2),
    all_distinct(Distances),
    length(Houses, NHouses),
    NConnections #= NHouses // 2,
    length(Connections, NConnections),
    %house -> x-y
    %connection -> [x1-y1, x2-y2]
    %dist(connection) = (x2-x1)^2 + (y2-y1)^2
    ensure1(Connections, Distances),
    %append connections = List of houses
    append(Connections, FlattenedCon),
    ensure2(Houses, FlattenedCon),
    ensure3(Connections),
    append(FlattenedCon, SuperFlattenedCon),
    labeling([], SuperFlattenedCon),
    write(Connections).

ensure1([], _).
ensure1([[[X1, Y1], [X2, Y2]]|Connections], Distances):-
    DiffX #= X2 - X1,
    DiffY #= Y2 - Y1,
    Dis #= DiffX * DiffX + DiffY * DiffY,
    member(Dis, Distances),
    ensure1(Connections, Distances).

ensure2([], _).
ensure2([H|Houses], FlattenedCon):-
    member(H, FlattenedCon),
    ensure2(Houses, FlattenedCon).

ensure3([_]).
ensure3([[[X1, Y1], [X2, Y2]], [[X3, Y3], [X4, Y4]]|Connections]):-
    X1 #=< X2,
    X1 #=< X3,
    X3 #=< X4,
    ensure3([[[X3, Y3], [X4, Y4]]|Connections]).