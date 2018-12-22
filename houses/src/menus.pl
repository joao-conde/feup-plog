%%%%%%%%%%%%%%%%%%%%%%%
%    MENUS MODULE     %
%%%%%%%%%%%%%%%%%%%%%%%

/* Main menu options */
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
	write('=        Houses Puzzles         =\n'),
	write('=================================\n'),
	write('=                               =\n'),
	write('=   1. Solve Puzzles            =\n'),
	write('=   2. About                    =\n'),
	write('=   3. Exit                     =\n'),
	write('=                               =\n'),
	write('=================================\n'),
	write('Choose an option:\t').


/* About display */
about:-
	print_about,
	request_enter,
	main_menu.

print_about:-
	clear_console,
	write('================================================================\n'),
	write('=                            ABOUT                             =\n'),
	write('================================================================\n'),
	write('=   In Houses Puzzles the objective is to connect pairs of     =\n'),								
	write('=   houses so that there are only two different distances.     =\n'),
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


/* Game mode menu options */
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



/* Game mode menu options */
solve_puzzles_prompt:-
	print_puzzle_to_solve(PuzzleName),
	write('Solve puzzle '),
	Term =.. [PuzzleName, Houses],
	Term, 
	connect(Houses),
	request_enter,
	puzzles_menu.

print_puzzle_to_solve(PuzzleName):-
	write('Puzzle name:\t'),
	get_word(PuzzleName, '').



/* Game mode menu options */
generate_puzzles_prompt:-
	print_generate_puzzle(PuzzleName, NHouses, Domain),
	write('New puzzle name'), write(PuzzleName), nl,
	generate(Houses, NHouses, Domain),
	savePuzzle(Houses, PuzzleName),
	request_enter,
	puzzles_menu.

print_generate_puzzle(PuzzleName, NHouses, Domain):-
	write('New puzzle name:\t'),
	get_word(PuzzleName, ''),
	write('New puzzle number of houses:\t'),
	get_int(NHouses),
	write('New puzzle size:\t'),
	get_int(Domain).

