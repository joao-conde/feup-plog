%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%      HOUSE PUZZLES: https://www2.stetson.edu/~efriedma/puzzle/house/       %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- use_module(library(lists)).
:- use_module(library(clpfd)).

:- include('board.pl').
:- include('cli.pl').
:- include('menus.pl').
:- include('puzzle.pl').

/* Application entry point */
start :- main_menu.

con :- consult('houses.pl').