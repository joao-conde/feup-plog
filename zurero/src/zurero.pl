%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%   ZURERO GAME RULES AT: http://www.iggamecenter.com/info/en/zurero.html    %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- use_module(library(lists)).
:- use_module(library(random)).

:- include('ai.pl').
:- include('board.pl').
:- include('cli.pl').
:- include('game.pl').
:- include('menus.pl').
:- include('utils.pl').

/* Application entry point */
play:- main_menu.