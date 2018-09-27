:- use_module(library(lists)).
:- use_module(library(random)).

:- include('menus.pl').
:- include('utils.pl').
:- include('board.pl').
:- include('botting.pl').

racingkings:-
	splashScreen.

playerVPlayer:-
	initialBoard(T),
	lineNumbers(Y),
	printBoard(T,Y),
	nl,
	player1(PlayerInit),
	mainPlayerVPlayer(T, PlayerInit).

playerVBot:-
	initialBoard(T),
	lineNumbers(Y),
	printBoard(T,Y),
	nl,
	mainPlayerVBot(T).

playerVBotHard:-
	initialBoard(T),
	lineNumbers(Y),
	printBoard(T,Y),
	nl,
	mainPlayerVBotHard(T).

botVBot:-
	initialBoard(T),
	lineNumbers(Y),
	printBoard(T,Y),
	nl, write('White Bot\'s turn'), nl ,pressEnterToContinue,
	mainBotVBot(T).

mainPlayerVPlayer(Board, CurrentPlayer):-
	playerMove(Board, CurrentPlayer, UpdatedBoard),
	ite((CurrentPlayer == white), player2(NextPlayer), player1(NextPlayer)),
	ite((CurrentPlayer == black), gameOver(UpdatedBoard), true),
	mainPlayerVPlayer(UpdatedBoard, NextPlayer).

mainPlayerVBot(Board):-
	playerMove(Board, UpdatedBoard),
	botMove(UpdatedBoard, UpdatedBotBoard, 1),
	gameOver(UpdatedBoard),
	mainPlayerVBot(UpdatedBotBoard).


mainPlayerVBotHard(Board):-
	playerMove(Board, UpdatedBoard),
	botHardMove(UpdatedBoard, UpdatedBotBoard, 1),
	gameOver(UpdatedBoard),
	mainPlayerVBotHard(UpdatedBotBoard).


mainBotVBot(Board):-
	botMove(Board, UpdatedBoard, 0),
	botMove(UpdatedBoard, UpdatedBotBoard, 1),
	gameOver(UpdatedBoard),
	mainBotVBot(UpdatedBotBoard).

playerMove(Board1, P, UpdatedBoard):-
  lineNumbers(Y1),
	playerTurn(P, PlayerName),
	write(PlayerName), write('\'s turn'),nl,
	getPiecePos(C,R),
	AbsR is abs(7 - R),
	getPiece(C, AbsR, Board1, Piece),
	Piece \= 0,
	ite((P==white),Piece =< 5, Piece >= 6),
	once(getDestPos(DesC,DesR)),
	AbsDesR is abs(7 - DesR),
	validateMove(Piece,AbsR,C,AbsDesR,DesC, Board1),

	setPiece(C, AbsR, 0, Board1, Board2),
	setPiece(DesC, AbsDesR, Piece, Board2, UpdatedBoard),
/*
	%check for kings check
	getPiece(BlackKingC, BlackKingR, UpdatedBoard, 6),
	getPiece(WhiteKingC, WhiteKingR, UpdatedBoard, 1),

	NewBoard = UpdatedBoard,
	ite(checkForCheck(8, UpdatedBoard, WhiteKingR, WhiteKingC, BlackKingR, BlackKingC, NewBoard),true,(!,fail)),
*/
	clearScreen,
	printBoard(UpdatedBoard, Y1),
	nl, pressEnterToContinue.

playerMove(Board, UpdatedBoard):-
  lineNumbers(Y1),
	write('Your turn'),nl,
	getPiecePos(C,R),
	AbsR is abs(7 - R),
	getPiece(C, AbsR, Board, Piece),
	Piece \= 0,	Piece =< 5,
	once(getDestPos(DesC, DesR)),
	AbsDesR is abs(7 - DesR),
	validateMove(Piece, AbsR, C, AbsDesR, DesC, Board),
	setPiece(C, AbsR, 0, Board, Board2),
	setPiece(DesC, AbsDesR, Piece, Board2, UpdatedBoard),
	clearScreen,
	printBoard(UpdatedBoard, Y1),
	nl, pressEnterToContinue.

getPiece(Col, Row, Board, Piece):-
	nth0(Row, Board, RowList),
	nth0(Col, RowList, Piece).

setPiece(ElemCol, 0, NewElem, [RowAtTheHead|RemainingRows], [NewRowAtTheHead|RemainingRows]):-
	setPieceList(ElemCol, NewElem, RowAtTheHead, NewRowAtTheHead).

setPiece(ElemCol, ElemRow, NewElem, [RowAtTheHead|RemainingRows], [RowAtTheHead|ResultRemainingRows]):-
	ElemRow > 0,
	ElemRow1 is ElemRow-1,
	setPiece(ElemCol, ElemRow1, NewElem, RemainingRows, ResultRemainingRows).

setPieceList(0, Elem, [_|L], [Elem|L]).
setPieceList(I, Elem, [H|L], [H|ResL]):-
	I > 0,
	I1 is I-1,
	setPieceList(I1, Elem, L, ResL), !.


gameOverMessage:-
	write('GAME OVER'),nl,
	pressEnterToContinue,
	mainMenu.

drawMessage:-
	write('It\'s a tie.'),nl,
	pressEnterToContinue,
	mainMenu.

gameOver(Board):-
	nth0(0, Board, LastRow),
	ite((member(6,LastRow) ; member(1,LastRow)),ite((member(6,LastRow) , member(1,LastRow)), drawMessage, gameOverMessage), true).
