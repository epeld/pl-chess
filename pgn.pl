:- module(pgn, []).

:- set_prolog_flag(double_quotes, codes).

:- use_module(pgn).
:- use_module(movement).

simplified_move(P, Mv, Simplified) :-
  full_move(P, Mv, FMv),
  setof(Mv0,
        (
          full_move(P, Mv0, FMv),
          unique_full_move(P, Mv0, FMv)
        ),
        [Simplified | _]).

make_move(Mv, P, P2) :-
  unique_full_move(P, Mv, FullMove),
  legal_position_after(FullMove, P, P2).

pgn_string(Move, String) :-
  phrase(move(Move), String).

unique_full_move(Position, Move, FullMove) :-
  findall(FullMove0, full_move(Position, Move, FullMove0), FullMoves),
  FullMoves = [FullMove].

full_move(Position, Move, FullMove) :-
  Move = [move, Pt, _, Mt, Dest | Rest],
  FullMove = [move, Pt, SourceSquare, Mt, Dest | Rest],
  source_square(Move, Position, SourceSquare).


source_square([move, PieceType, Hint, MoveType, Destination], Position, SourceSquare) :-
  movement:officer(PieceType),
  source_square2(PieceType, Hint, MoveType, Destination, Position, SourceSquare).

source_square([move, pawn, Hint, MoveType, Destination, _Promo], Position, SourceSquare) :-
  source_square2(pawn, Hint, MoveType, Destination, Position, SourceSquare),
  pawn_hint(MoveType, Hint).


source_square2(PieceType, Hint, MoveType, Destination, Position, SourceSquare) :-
  fen:turn(Position, Color),
  compatible(Hint, SourceSquare),
  fen:piece_at(Position, SourceSquare, piece(PieceType, Color)),
  possible_move(MoveType, PieceType, SourceSquare, Destination, Position).


pawn_hint(move, nothing).
pawn_hint(capture, file(_F)).
pawn_hint(_Mt, square(_F, _R)).

compatible(square(X, Y), square(X, Y)).
compatible(file(File), square(File, _)).
compatible(rank(Rank), square(_, Rank)).
compatible(nothing, square(_X, _Y)).


possible_move(capture, pawn, Src, Dst, P) :-
  fen:piece_at(P, Src, piece(pawn, Color)),
  
  ( Pc = [_, Enemy], color:opposite(Color, Enemy)
  ; Pc = nothing, fen:passant(P, Dst) ),
  
  movement:pawn_capture_square(Color, Src, Dst),
  fen:piece_at(P, Dst, Pc).

possible_move(move, pawn, Src, Dst, P) :-
  fen:piece_at(P, Src, piece(pawn, Color)),

  fen:turn(P, Color),
  
  movement:pawn_move_square(Color, Src, Dst),
  movement:passant_square(Color, Src, Dst, Passant),
  
  ( fen:piece_at(P, Passant, nothing) ; Passant = nothing ),

  fen:piece_at(P, Dst, nothing).


possible_move(move, Officer, Src, Dst, P) :-
  fen:piece_at(P, Src, piece(Officer, _)),
  movement:officer(Officer),
  
  possible_move(Officer, Src, Dst, P),
  fen:piece_at(P, Dst, nothing).


possible_move(capture, Officer, Src, Dst, P) :-
  fen:piece_at(P, Src, piece(Officer, _)),
  movement:officer(Officer),
  
  fen:turn(P, Color),
  color:opposite(Color, Enemy),

  possible_move(Officer, Src, Dst, P),
  fen:piece_at(P, Dst, piece(_, Enemy)).

possible_move(DiagonalMover, Src, Dst, P) :-
  diagonal_mover(DiagonalMover),
  diagonal_move(Src, Dst, P, _).

possible_move(StraightMover, Src, Dst, P) :-
  straight_mover(StraightMover),
  straight_move(Src, Dst, P, _).


possible_move(king, Src, Dst, _P) :-
  movement:offset(Src, Dst, _).


possible_move(knight, Src, Dst, _P) :-
  movement:knights_jump(Src, Dst).


middle_is_nothing(P,Squares) :-
  append([ [_], Middle, [_] ], Squares),
  maplist(fen:piece_at(P), Middle, Pieces),
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
  source_hint(SourceHint), { \+ SourceHint == rank(_) } , 
  move_type(MoveType), 
  fen:square(Destination),
  promotion(Promotion).


move_type(move) --> [].
move_type(capture) --> "x".

source_hint(file(File)) -->
  {
    nth0(File, "abcdefgh", Char)
  },
  [Char].

source_hint(rank(Rank)) -->
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

castles(castles(queenside)) --> "O-O-O".
castles(castles(kingside)) --> "O-O".


check(Position, SourceSquare) :-
  Position = [position, Board, Color | Rest],

  % "If it were the opponents turn":
  color:opposite(Color, Opponent),
  Flipped = [position, Board, Opponent | Rest],

  % Is there a piece that can attack the king?
  fen:piece_at(Flipped, KingSquare, piece(king, Color)),
  attacker_of(Flipped, KingSquare, SourceSquare).


legal_position_after(FullMove, Position, Position2) :-
  fen:turn(Position, Color),

  % "In the position after FullMove":
  position:position_after(FullMove, Position, Position2),

  % "There are zero attackers of the king"
  fen:piece_at(Position2, KingSquare, piece(king, Color)),
  \+ attacker_of(Position2, KingSquare, _).

attacker_of(Position, AttackedSquare, SourceSquare) :-
  SourceSquare = square(_, _),
  full_move(Position, [move, _, SourceSquare, capture, AttackedSquare | _], _).


stalemate(Position) :-
  \+ check(Position, _),
  \+ legal_position_after(_, Position, _).


checkmate(Position) :-
  check(Position, _),
  !,
  \+ legal_position_after(_, Position, _).


castling_possible(Side, Position) :-
  Position = [position, Turn, _, _, _, _],
  
  color:initial_king_square(Turn, Start),
  color:castled_king_square(Turn, Side, End),
  movement:line(Start, End, Line, _),
  append([ [Start], Middle, [End] ], Line),

  
  member(Square, Middle),
  full_move(Position, [move, _, _, capture, Square | _], _),
  !.
   

diagonal(up_right).
diagonal(up_left).
diagonal(down_left).
diagonal(down_right).


straight(up).
straight(down).
straight(left).
straight(right).

%
%
%

diagonal_move(Src, Dst, _, Dir, 1) :-
  diagonal(Dir),
  movement:offset(Src, Dst, Dir).


diagonal_move(Src, Dst, P, Dir, N) :-
  succ(N0, N),
  diagonal_move(Src, Dst0, P, Dir, N0),
  movement:offset(Dst0, Dst, Dir),
  fen:piece_at(P, Dst0, nothing).

diagonal_move(Src, Dst, P, Dir) :-
  between(1, 8, N),
  diagonal_move(Src, Dst, P, Dir, N).


straight_move(Src, Dst, _, Dir, 1) :-
  straight(Dir),
  movement:offset(Src, Dst, Dir).


straight_move(Src, Dst, P, Dir, N) :-
  succ(N0, N),
  straight_move(Src, Dst0, P, Dir, N0),
  movement:offset(Dst0, Dst, Dir),
  fen:piece_at(P, Dst0, nothing).

straight_move(Src, Dst, P, Dir) :-
  between(1, 7, N),
  straight_move(Src, Dst, P, Dir, N).

castling_move(castles(Side), Side).
