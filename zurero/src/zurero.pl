%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%   ZURERO GAME RULES AT: http://www.iggamecenter.com/info/en/zurero.html    %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- use_module(library(lists)).

:- include('utils.pl').
:- include('menus.pl').
:- include('game.pl').
:- include('board.pl').

%game entry point
zurero:- main_menu.