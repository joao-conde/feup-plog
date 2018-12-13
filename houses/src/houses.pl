%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%      HOUSE PUZZLES: https://www2.stetson.edu/~efriedma/puzzle/house/       %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- use_module(library(lists)).

:- include('board.pl').
:- include('cli.pl').
:- include('menus.pl').
:- include('utils.pl').

/* Application entry point */
play:- main_menu.