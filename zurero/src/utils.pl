pressEnterToContinue:-
	write('Press <Enter> to continue.'), nl,
	waitForEnter, !.

waitForEnter:-
	get_char(_).


%clears console by printing multiple newlines 
clearConsole:- clearConsole(20), !.
clearConsole(0).
clearConsole(N):-
	nl,
	N1 is N-1,
	clearConsole(N1).


%reads the input char and the '\n' character left in input stream
getChar(Input):-
	get_char(Input),
	get_char(_).
