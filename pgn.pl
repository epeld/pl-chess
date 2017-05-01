:- module(pgn, []).


full_move(Move, FullMove) :-
  source_square(Move, SourceSquare),
  position:list_replace(2, SourceSquare, Move, FullMove).


source_square([move, PieceType, Hint, MoveType, Destination | _], Position, SourceSquare) :-
  square(SourceSquare),
  piece_at(Position, SourceSquare, [PieceType, Color]),
  turn(Position, Color),
  possible_move(MoveType, PieceType, SourceSquare, Destination, Position),
  compatible(SourceSquare, Hint).


compatible(Square, Square).
compatible([file, File], [square, _, File]).
compatible([rank, Rank], [square, Rank, _]).
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


%
%  Parsing
%

move(Castles) --> castles(Castles).

move([move, OfficerType, SourceHint, MoveType, Destination]) -->
  officer(OfficerType),
  source_hint(SourceHint),
  move_type(MoveType),
  fen:square(Destination).


move([move, pawn, SourceHint, MoveType, Destination, Promotion]) -->
  source_hint(SourceHint), { \+ SourceHint == [rank, _ ] } , 
  move_type(MoveType), 
  fen:square(Destination),
  promotion(Promotion).


move_type(move) --> [].
move_type(capture) --> "x".

source_hint([file, File]) -->
  {
    nth0(File, "abcdefgh", Char)
  },
  [Char].

source_hint([rank, Rank]) -->
  {
    nth0(Rank, "12345678", Char)
  },
  [Char].

source_hint(Square) -->
  fen:square(Square).


source_hint(nothing) --> [].


promotion(nothing) --> [].
promotion(OfficerType) -->
  "=",
  officer(OfficerType).

officer(OfficerType) -->
  {
    movement:officer(OfficerType),
    fen:piece_char([OfficerType, white], Char)
  },
  [Char].

castles([castles, queenside]) --> "O-O-O".
castles([castles, kingside]) --> "O-O".
