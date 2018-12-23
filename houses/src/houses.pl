:- use_module(library(lists)).
:- use_module(library(clpfd)).
:- use_module(library(random)).
:- use_module(library(file_systems)).

:- include('menus.pl').
:- include('cli.pl').

:- include('restrictions.pl').
:- include('puzzles.pl').
:- include('utils.pl').



houses:- main_menu.

/*
    let's assume ALL LISTS FLAT 
        -houses is a list of integers, each 2 integers is (X,Y) of an house
        -connections refer to houses using indexes of houses list
        -connections are not a list of lists, but a list of integers, and each 2 consecutive integers  are indexes to house and represent
        a connection
*/
connect(Houses):-
    length(Houses, NHouses),
    append(Houses, FlatHouses),
    max_member(Max, FlatHouses),

    length(Connections, NHouses),
    domain(Connections, 1, NHouses),
    all_distinct(Connections),

    %2 different distances
    length(Distances, 2),
    all_distinct(Distances),
    MaxDis is 2 * Max * Max,
    domain(Distances, 1, MaxDis),

    % write(Connections), nl,
    restrictConnectDistances(Connections, FlatHouses, Distances),

    append(Distances, Connections, Vars),
    labeling([], Vars),
    % statistics,
    write('---Make the following connections---'), nl,
    printSolution(Houses, Connections),
    write('---End---'), nl.



%generator ---------------------------------------------------
generate(Houses, NHouses, Domain):-
    NFlatHouses is NHouses * 2,
    length(FlatHouses, NFlatHouses),
    domain(FlatHouses, 0, Domain),
    ensureDifferentHouses(FlatHouses),

    length(Connections, NHouses),
    domain(Connections, 1, NHouses),
    all_distinct(Connections),
    
    MaxDis is 2 * Domain * Domain,
    length(Distances, 2),
    domain(Distances, 1, MaxDis),
    all_distinct(Distances),    

    restrictConnectDistances(Connections, FlatHouses, Distances),

    append(FlatHouses, Connections, Vars),
    append(Vars, Distances, Vars2),
    labeling([value(randomLabeling)], Vars2),
    buildHouseList(FlatHouses, Houses, []).
