/*
  0 - empty space
  1 - white king
  2 - white queen
  3 - white tower
  4 - white bishop
  5 - white knight
  6 - black king
  7 - black queen
  8 - black tower
  9 - black bishop
 10 - black knight
*/

/* Associates each number with a chess piece */
translate(0,' .. ').
translate(1,' WK '). %King
translate(2,' WQ '). %Queen
translate(3,' WR '). %Rook
translate(4,' WB '). %Bishop
translate(5,' Wk '). %Knight
translate(6,' BK '). %King
translate(7,' BQ '). %Queen
translate(8,' BR '). %Rook
translate(9,' BB '). %Bishop
translate(10,' Bk '). %Knight


columnToInt('A', 0).
columnToInt('B', 1).
columnToInt('C', 2).
columnToInt('D', 3).
columnToInt('E', 4).
columnToInt('F', 5).
columnToInt('G', 6).
columnToInt('H', 7).
columnToInt('a', 0).
columnToInt('b', 1).
columnToInt('c', 2).
columnToInt('d', 3).
columnToInt('e', 4).
columnToInt('f', 5).
columnToInt('g', 6).
columnToInt('h', 7).

/* Starting game board */
initialBoard([[0,0,0,0,0,0,0,0],
              [6,0,0,0,0,0,0,1],
              [0,0,0,0,0,0,0,0],
              [0,0,0,0,0,0,0,0],
              [0,0,0,0,0,0,0,0],
              [0,0,0,0,0,0,0,0],
              [6,8,9,10,5,4,3,1],
              [7,8,9,10,5,4,3,2]]).


lineNumbers([8,7,6,5,4,3,2,1]).

clearScreen:-write('\e[2J').

pressEnterToContinue:-
	write('Press <Enter> to continue.'), nl,
	waitForEnter, !.

waitForEnter:-
	get_char(_).

getChar(Input):-
	get_char(Input),
  write(Input),nl,
	get_char(_).

getCode(Input):-
	get_code(TempInput),
	get_code(_),
	Input is TempInput - 48.

getInt(Input):-
	get_code(TempInput),
  get_code(_),
	Input is TempInput - 48.

discardInputChar:-
	get_code(_).

/* Recursive function to print a ASCII CODE a specific number of times */
%Ended up not being used since there was a need to use a specific font that I didn't like.
putCode(Times, Code) :-
      Times > 0, !,
      put_code(Code),
      TimesN is Times - 1,
      putCode(TimesN, Code).

putCode(0,[]).

/*IF-THEN-ELSE*/
ite(If,Then,_Else):-
  If,!,Then.

ite(_If,_Then,Else):-
  Else.
