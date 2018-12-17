%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%    COMMAND-LINE INTERFACE MODULE     %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

/*  clear_console
    
    Simulates a console clear by printing out 100 newline characters ('\n').
*/
clear_console:- 
    clear_console(100), !.


/*  clear_console(+N)
    
    Prints N newline characters ('\n') on console.
*/
clear_console(0).
clear_console(N):-
	nl,
	N1 is N-1,
	clear_console(N1).

/*  request_enter
    
    Requests user to press Enter key to advance.
    Reads the remaining newline character ('\n') in the console.
*/
request_enter:-
	write('Press <Enter> to continue.\n'),
	get_char(_), !.

/*  get_char_nl(-Input)
    
    Reads a character from console unifying it with Input.
    Reads the remaining newline character ('\n') in the console.
*/
get_char_nl(Input):-
	get_char(Input),
	get_char(_).


/*  get_int(-Input)
    
    Reads a number up to 2-digits and unifies it with Input.
*/
get_int(Input):-
    get_code(FirstChar),
    get_code(SecondChar),
    aux_get_int(FirstChar, SecondChar, Input).

/* SecondChar code is 10 i.e. '\n' character code. 1-digit number, Input unified with it. */
aux_get_int(FirstChar, 10, Input):-
    Input is FirstChar - 48.

/*  SecondChar code is not 10 i.e. not '\n' character code. 2-digit number, Input unified with it */
aux_get_int(FirstChar, SecondChar, Input):-
    FirstDigit is FirstChar - 48,
    SecondDigit is SecondChar - 48,
    Temp is FirstDigit * 10,
    Input is Temp + SecondDigit,
    get_char(_).