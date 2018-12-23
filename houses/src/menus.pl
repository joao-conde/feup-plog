%%%%%%%%%%%%%%%%%%%%%%%
%    MENUS MODULE     %
%%%%%%%%%%%%%%%%%%%%%%%

/* Main menu */
main_menu:-
	print_main_menu,
	get_int(Input),
	main_menu_option(Input).

main_menu_option(1):- 
	puzzles_menu.

main_menu_option(2):- 
	about.

/* Does nothing, terminates program */
main_menu_option(3).

main_menu_option(_):- 
	write('\nError: invalid input.\n'),
	request_enter,
	main_menu.

print_main_menu:-
	clear_console,
	write('=================================\n'),
	write('=             Houses            =\n'),
	write('=================================\n'),
	write('=                               =\n'),
	write('=   1. Solve Puzzles            =\n'),
	write('=   2. About                    =\n'),
	write('=   3. Exit                     =\n'),
	write('=                               =\n'),
	write('=================================\n'),
	write('Choose an option:\t').


/* About */
about:-
	print_about,
	request_enter,
	main_menu.

print_about:-
	clear_console,
	write('================================================================\n'),
	write('=                            ABOUT                             =\n'),
	write('================================================================\n'),
	write('=   In Houses the objective is to connect pairs of houses so   =\n'),								
	write('=   that there are only two different distances.               =\n'),
	write('=                                                              =\n'),
	write('=   All houses must be connected to a single different house.  =\n'),
	write('=                                                              =\n'),
	write('=   Each puzzle below has a unique solution.                   =\n'),
	write('=                                                              =\n'),
	write('=   Some pre-loaded puzzles were created by Erich              =\n'),
	write('=   Friedman, 2008.                                            =\n'),
	write('=                                                              =\n'),
	write('=   Houses Puzzles was developed for the course of             =\n'),
	write('=   Logic Programming.                                         =\n'),
	write('=                                                              =\n'),
	write('=   It is written entirely in SICStus PROLOG using the CLPFD   =\n'),
	write('=   library (Constraing Logic Programming with Finite Domains) =\n'),
	write('=                                                              =\n'),
	write('=   Developed by:                                              =\n'),
	write('=    > Afonso Ramos                                            =\n'),
	write('=    > Joao Conde                                              =\n'),
	write('=                                                              =\n'),
	write('================================================================\n').


/* Puzzles menu */
puzzles_menu:-
	print_puzzles_menu,
	get_int(Input),
	puzzles_menu_option(Input).

puzzles_menu_option(1):-
	solve_puzzles_prompt.

puzzles_menu_option(2):-
	generate_puzzles_prompt.

puzzles_menu_option(3):- 
	main_menu.

puzzles_menu_option(_):- 
	write('\nError: invalid input.\n'),
	request_enter,
	puzzles_menu.

print_puzzles_menu:-
	clear_console,
	write('=================================\n'),
	write('=         Puzzles Menu          =\n'),
	write('=================================\n'),
	write('=                               =\n'),
	write('=   1. Solve saved puzzles      =\n'),
	write('=   2. Generate new puzzle      =\n'),
	write('=   3. Back                     =\n'),
	write('=                               =\n'),
	write('=================================\n'),
	write('Choose an option:\t').


/* Solver interface */
solve_puzzles_prompt:-
	print_puzzle_to_solve(PuzzleName), nl,
	Term =.. [PuzzleName, Houses],
	Term,
	nl, write('This might take a few minutes... or not :D'), nl,
	connect(Houses), nl,
	request_enter,
	puzzles_menu.

print_puzzle_to_solve(PuzzleName):-
	write('Enter puzzle name:\t'), 
	get_word(PuzzleName, '').


/* Generator interface */
generate_puzzles_prompt:-
	print_generate_puzzle(PuzzleName, NHouses, Domain), nl,
	generate(Houses, NHouses, Domain),
	nl, write('This might take a few minutes... or not :D'), nl,
	savePuzzle(Houses, PuzzleName),
	write(PuzzleName), write(' saved successfully'), nl, nl,
	request_enter,
	puzzles_menu.

print_generate_puzzle(PuzzleName, NHouses, Domain):-
	write('Enter new UNIQUE puzzle name:\t'),
	get_word(PuzzleName, ''),
	write('Enter EVEN number of houses:\t'),
	get_int(NHouses),
	write('Enter coordinates domain upper bound (> 1):\t'),
	get_int(Domain).