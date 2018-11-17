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


/*  print_direction_msg
    
    Prints the direction input request from user.
*/
print_direction_msg:-
	write('\nSliding stone direction?\n'),
	write('1 - TOP\n'),
	write('2 - RIGHT\n'),
	write('3 - BOT\n'),
	write('4 - LEFT\n').


/*  print_coord_msg
    
    Prints the coords input request from user.
*/
print_coord_msg:-
	write('Where to throw it?\t').


/*  print_msg_piece_mid_placed(+Player)
    
    Prints the information that the Player's piece was automatically placed in the middle of the board.
*/
print_msg_piece_mid_placed(Player):-
    write('First piece placed in the middle of the board for '), write(Player), write(' player\n').


/*  print_msg_win_game(+Player)
    
    Prints the information that Player won the game.
*/
print_msg_win_game(Player):-
    write('Congratulations, '), write(Player), write('!\nYou won the game!\n').


/*  print_player_turn(+Player)
    
    Prints that the current turn belongs to Player.
*/
print_player_turn(Player):-
    write(Player), write(' \'s turn\n').


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


/*  get_direction(-Direction)
    
    Requests user for a sliding stone direction and unifies Direction with it.
*/
get_direction(Direction):-
	repeat,
	print_direction_msg,
	get_int(Input),
	direction(Input, Direction).


/*  get_vertical_coord(-Col)
    
    Requests user for a vertical slide coord, which is a column index, unified with Col.
    User input is a letter so it is converted to a 0-based index.
*/
get_vertical_coord(Col):-
	repeat,
	print_coord_msg,
	get_char_nl(ColChar),
	letter_to_int(ColChar, Col),
	Col >= 0,
	Col =< 18.


/*  get_horizontal_coord(-InvLine)
    
    Requests user for an horizontal slide coord, which is a line index, unified with InvLine.
    User input is a number, the number of the line, but as he sees the board indexes reversed,
    we must reverse the input, unifying it with InvLine.
*/
get_horizontal_coord(InvLine):-
	repeat,
	print_coord_msg,
	get_int(Line1),
	Line1 >= 1, 
	Line1 =< 19,
	Line is Line1 - 1,
	InvLine is abs(18 - Line).


/*  get_coord(-Col, top)
    
    Requests for a column index, unifying it with Col.
*/
get_coord(Col, top):-
	get_vertical_coord(Col).


/*  get_coord(-Col, bot)
    
    Requests for a column index, unifying it with Col.
*/
get_coord(Col, bot):-
	get_vertical_coord(Col).


/*  get_coord(-InvLine, left)
    
    Requests for a line index, unifying it with InvLine.
*/
get_coord(InvLine, left):-
	get_horizontal_coord(InvLine).


/*  get_coord(-InvLine, right)
    
    Requests for a line index, unifying it with InvLine.
*/
get_coord(InvLine, right):-
	get_horizontal_coord(InvLine).