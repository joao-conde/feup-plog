botMove(Board, UpdatedBoard, Color) :-
  lineNumbers(Y1),
  %trace,
  randomMove(C, R, DesC, DesR, Board, Piece, Color),
  write(R),
  %AbsR is abs(7 - R),
	setPiece(C, R, 0, Board, Board2),
  %notrace,
	%NewDesR is abs(7 - DesR),
	setPiece(DesC, DesR, Piece, Board2, UpdatedBoard),
	clearScreen,
  printBoard(UpdatedBoard, Y1),
  nl, ite(Color == 0,(write('Black Bot\'s turn')), (write('White Bot\'s turn'))),
	nl, pressEnterToContinue.


botHardMove(Board, UpdatedBoard, Color) :-
  lineNumbers(Y1),
  ite(randomAdvancedMove(C, R, DesC, DesR, Board),Piece = 6,
      randomMove(C, R, DesC, DesR, Board, Piece, Color)),
  setPiece(C, R, 0, Board, Board2),
  setPiece(DesC, DesR, Piece, Board2, UpdatedBoard),
  write(DesC),nl, write(DesR),  nl,
  %clearScreen,
  printBoard(UpdatedBoard, Y1),
  nl, write('Black Bot\'s turn'),
  nl, pressEnterToContinue.


randomAdvancedMove(Col, Row, DesC, DesR, Board):-
  getPiece(Col, Row, Board, 6),
  DesC is Col,
  DesR is (Row-1),
  validateMove(6, Row, Col, DesR, DesC, Board).


randomMove(Col, Row, DesC, DesR, Board, Piece, Color) :-
  randomPiece(Col,Row,Board,Piece, Color),
  randomDestination(Col, Row, DesC, DesR, Board, Piece).


randomDestination(Col, Row, DesC, DesR, Board, Piece):-
  repeat,
  random(0, 8, DesC),
  random(0, 8, WrongDesR),
  DesR is abs(7 - WrongDesR),
  validateMove(Piece, Row, Col, DesR, DesC, Board).

randomPiece(Col, Row, Board, Piece, Color):-
  repeat,
  random(0, 8, Col),
  random(0, 8, WrongRow),
  Row is abs(7 - WrongRow),
  getPiece(Col, Row, Board, Piece),
	Piece \= 0,
  ite(Color == 1,Piece >= 6, Piece =< 5).

%randomPiece(Col, Row, Board, Piece).
