:- module(pgn, []).


source_square([move, PieceType, Hint, MoveType, Destination], Position, SourceSquare) :-
  square(SourceSquare),
  piece_at(Position, SourceSquare, [PieceType, Color]),
  turn(Position, Color),
  possible_move(MoveType, PieceType, SourceSquare, Destination, Position),
  compatible(SourceSquare, Hint).


compatible([file, File], Square) :-
  square_codes(Square, [File, _]).


compatible([rank, Rank], Square) :-
  square_codes(Square, [_, Rank]).


compatible(Square, Square).

compatible(nothing, _Square).


possible_move(capture, pawn, Src, Dst, P) :-
  ( piece_at(P, Dst, [_, Enemy]), turn(P, Color), opposite(Color, Enemy)
  ; passant(P, Dst) ),
  
  pawn_capture_square(Color, Src, Dst).

possible_move(move, pawn, Src, Dst, P) :-
  piece_at(P, Dst, nothing),

  pawn_move_square(Color, Src, Dst),
  turn(P, Color),
  
  line(Src, Dst, Line, _),
  append([[Src], Middle, [Dst]], Line),
  maplist(=(nothing), Middle).


possible_move(move, Officer, Src, Dst, P) :-
  officer(Officer),
  piece_at(P, Dst, nothing),

  possible_move(Officer, Src, Dst, P).


possible_move(capture, Officer, Src, Dst, P) :-
  officer(Officer),
  piece_at(P, Dst, [_, Enemy]),

  turn(P, Color),
  opposite(Color, Enemy),

  possible_move(Officer, Src, Dst, P).


possible_move(DiagonalMover, Src, Dst, P) :-
  diagonal_mover(DiagonalMover),
  diagonal(Src, Dst, Diagonal, _),
  middle_is_nothing(P,Diagonal).


possible_move(StraightMover, Src, Dst, P) :-
  straight_mover(StraightMover),
  line(Src, Dst, Line, _),
  middle_is_nothing(P,Line).


possible_move(king, Src, Dst, _P) :-
  diagonal(Src, Dst, Diagonal, _),
  length(Diagonal, 2).


possible_move(king, Src, Dst, _P) :-
  line(Src, Dst, Diagonal, _),
  length(Diagonal, 2).


possible_move(knight, Src, Dst, _P) :-
  knights_jump(Src, Dst).


middle_is_nothing(P,Squares) :-
  append([ [_], Middle, [_] ], Squares),
  maplist(piece_at(P), Middle, Pieces),
  maplist(=(nothing), Pieces).


diagonal_mover(bishop).
diagonal_mover(queen).

straight_mover(queen).
straight_mover(rook).
