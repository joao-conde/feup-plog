printInitialBoard :-
    initialBoard(X),
    lineNumbers(Y),
    printBoard(X,Y).

printMidBoard :-
    midgameBoard(X),
    lineNumbers(Y),
    printBoard(X,Y).

printEndBoard :-
    endgameBoard(X),
    lineNumbers(Y),
    printBoard(X,Y).

player1(white).
player2(black).

playerTurn(white, 'White').
playerTurn(black, 'Black').

/* Recursive function to print current board state */
printBoard([],[]) :-
    write('  |----|----|----|----|----|----|----|----|'), nl,
    write('    A    B    C    D    E    F    G    H   ').

/*printBoard([],[]) :-
    write('  '), putCode(25,205), nl,
    write('   A  B  C  D  E  F  G  H ').*/

printBoard([Line|Board],[LineNumb|Remainder]) :-
    write('  |----|----|----|----|----|----|----|----|'), nl,
    %write('  '), putCode(25,205),  nl,
    write(LineNumb), write(' '),
    printLine(Line),
    write('|'), nl,
    printBoard(Board,Remainder).

/* Recursive function to print each board's line */
printLine([]).
printLine([Head|Tail]) :-
    translate(Head,T),
    write('|'),
    %put_code(186),
    write(T),
    printLine(Tail).

/* Get element at (row,col) */
getPiecePos(Column, Row):-
	repeat,
    write('Choose which piece to move.'), nl,
	write('Column:'),
	getChar(Char),
	columnToInt(Char, Column),
	write('Row:'),
	getInt(R),
	R =< 8,
	R >= 1,
	Row is R - 1.


getDestPos(Column, Row):-
      repeat,
      write('Choose where to move.'), nl,
      write('Column:'),
      getChar(Char),
      columnToInt(Char, Column),
      write('Row:'),
      getInt(R),
      R =< 8,
      R >= 1,
      Row is R - 1.

/* MOVES VALIDATION*/

%Generic validate
validateMove(Piece, R, C, DesR, DesC, Board):-
      getPiece(DesC, DesR, Board, DesPiece),

      %Check if destiny has friendly piece.
      ite(Piece =< 5, (DesPiece >= 6 ; DesPiece == 0), true),
      ite(Piece >= 6, DesPiece =< 5, true),

      %lmao
      DesPiece \= 1,
      DesPiece \= 6,

      %Check if movement is allowed.
      ite((Piece==1 ; Piece==6),validateKingMove(R, C, DesR, DesC),true),
      ite((Piece==2 ; Piece==7),validateQueenMove(R, C, DesR, DesC),true),
      ite((Piece==3 ; Piece==8),validateRookMove(R, C, DesR, DesC),true),
      ite((Piece==4 ; Piece==9),validateBishopMove(R, C, DesR, DesC),true),
      ite((Piece==5 ; Piece==10),validateKnightMove(R, C, DesR, DesC),true).




checkForCheck(0,_,_,_,_,_,_,_,_).
checkForCheck(Rows, [Head|Tail], WKingR, WKingC, BKingR, BKingC, Board) :-
    Rows > 0,
    Rows1 is Rows-1,
    ite((checkForCheckList(8, Head, WKingR, WKingC, BKingR, BKingC, Board),write('PORQUE')),(write('sdaf'),true),(!,fail)),
    checkForCheck(Rows1,Tail, WKingR, WKingC, BKingR, BKingC, Board).


checkForCheckList(0,[],_,_,_,_,_,_,_).
checkForCheckList(Cols, [Head|Tail], WKingR, WKingC, BKingR, BKingC, Board) :-
      Cols > 0,
      Cols1 is (Cols - 1),
      write('CHECKLIST: '),write(Cols1),nl,
      Head \= 1, Head \= 6,
      getPiece(PieceC, PieceR, Board, Head),
      write(Head),nl,write(PieceR),nl,write(PieceC),nl,write(BKingR),nl,write(BKingC),nl,write(WKingR),nl,write(WKingC),nl,
      pressEnterToContinue,
      ite(Head =< 5,ite(validateMove(Head,PieceR, PieceC,BKingR,BKingC,Board),(write('FAIL'),fail),true),true),
      ite(Head >= 6,ite(validateMove(Head,PieceR, PieceC,WKingR,WKingC,Board),(write('FAIL'),fail),true),true),

      checkForCheckList(Cols1, Tail, WKingR, WKingC, BKingR, BKingC, Board).


validateKingMove(KingR,KingC,Row,Col):-
      Row =< (KingR + 1),
      Row >= (KingR - 1),
      Col >= (KingC - 1),
      Col =< (KingC + 1).

validateRookMove(RookR,RookC,Row,Col):-
      RookC =:= Col ; RookR =:= Row.


validateQueenMove(QueenR,QueenC,Row,Col):-
  validateRookMove(QueenR,QueenC,Row,Col) ;
  validateBishopMove(QueenR,QueenC,Row,Col).


validateBishopMove(BishopR,BishopC,Row,Col):-
      %Distance is 0,
      %maxDistanceBishop(Distance,BishopR,BishopC,Row,Col,Board), %doenst allow to skip pieces (should)
      DifR is abs(BishopR - Row),
      DifC is abs(BishopC - Col),
      %DifC < Distance, DifR < Distance,
      BishopC \= Col , BishopR \= Row,
      DifC == DifR.

validateKnightMove(KnightR,KnightC,Row,Col):-
      (Row =:= (KnightR + 2), Col =:= (KnightC - 1));
      (Row =:= (KnightR + 1), Col =:= (KnightC - 2));
      (Row =:= (KnightR + 2), Col =:= (KnightC + 1));
      (Row =:= (KnightR + 1), Col =:= (KnightC + 2));
      (Row =:= (KnightR - 2), Col =:= (KnightC - 1));
      (Row =:= (KnightR - 1), Col =:= (KnightC - 2));
      (Row =:= (KnightR - 2), Col =:= (KnightC + 1));
      (Row =:= (KnightR - 1), Col =:= (KnightC + 2)).


%max distance allowed to travel calculator for bishop
maxDistanceBishop(MaxDistance,BishopR,BishopC,Row,Col,Board):-
    DeltaR is (Row - BishopR),
    DeltaC is (Col - BishopC),
    ite(DeltaC > 0,
            ite(DeltaR > 0, maxDistanceBishopDir1(MaxDistance,BishopR,BishopC,Row,Col,Board), maxDistanceBishopDir2(MaxDistance,BishopR,BishopC,Row,Col,Board)),
            ite(DeltaR > 0, maxDistanceBishopDir4(MaxDistance,BishopR,BishopC,Row,Col,Board), maxDistanceBishopDir3(MaxDistance,BishopR,BishopC,Row,Col,Board))).


maxDistanceBishopDir1(MaxDistance,BishopR,BishopC,Row,Col,Board):-
  BishopR < Row,
  BishopC < Col,
  NBishopC is (BishopC + 1),
  NBishopR is (BishopR + 1),
  getPiece(NBishopC,NBishopR,Board,Piece),
  Piece == 0,
  MaxDistance1 is (MaxDistance + 1),
  maxDistanceBishopDir1(MaxDistance1,NBishopC,NBishopR,Row,Col,Board).

maxDistanceBishopDir2(MaxDistance,BishopR,BishopC,Row,Col,Board):-
  BishopR > Row,
  BishopC < Col,
  NBishopC is (BishopC + 1),
  NBishopR is (BishopR - 1),
  getPiece(NBishopC,NBishopR,Board,Piece),
  Piece == 0,
  MaxDistance1 is (MaxDistance + 1),
  maxDistanceBishopDir2(MaxDistance1,NBishopC,NBishopR,Row,Col,Board).


maxDistanceBishopDir3(MaxDistance,BishopR,BishopC,Row,Col,Board):-
  BishopR > Row,
  BishopC > Col,
  NBishopC is (BishopC - 1),
  NBishopR is (BishopR - 1),
  getPiece(NBishopC,NBishopR,Board,Piece),
  Piece == 0,
  MaxDistance1 is (MaxDistance + 1),
  maxDistanceBishopDir3(MaxDistance1,NBishopC,NBishopR,Row,Col,Board).


maxDistanceBishopDir4(MaxDistance,BishopR,BishopC,Row,Col,Board):-
  BishopR < Row,
  BishopC > Col,
  NBishopC is (BishopC - 1),
  NBishopR is (BishopR + 1),
  getPiece(NBishopC,NBishopR,Board,Piece),
  Piece == 0,
  MaxDistance1 is (MaxDistance + 1),
  maxDistanceBishopDir4(MaxDistance1,NBishopC,NBishopR,Row,Col,Board).
