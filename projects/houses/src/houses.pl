/* PUZZLE PROBLEM: https://www2.stetson.edu/~efriedma/puzzle/house/ */

:- use_module(library(lists)).
:- use_module(library(clpfd)).
:- use_module(library(random)).
:- use_module(library(file_systems)).

:- include('menus.pl').
:- include('cli.pl').
:- include('restrictions.pl').
:- include('puzzles.pl').
:- include('utils.pl').

/* Application entry point */
houses:- main_menu.