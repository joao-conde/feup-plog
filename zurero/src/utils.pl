request_enter:-
	write('Press <Enter> to continue.'), nl,
	wait_for_enter, !.

wait_for_enter:-
	get_char(_).


%clears console by printing multiple newlines 
clear_console:- clear_console(20), !.
clear_console(0).
clear_console(N):-
	nl,
	N1 is N-1,
	clear_console(N1).


%reads the input char and the '\n' character left in input stream
get_char_nl(Input):-
	get_char(Input),
	get_char(_).
