:- module(color, [color/1]).

color(white).
color(black).

opposite(white, black).
opposite(black, white).

last_pawn_rank(white, 7).
last_pawn_rank(black, 0).

first_pawn_rank(white, 2).
first_pawn_rank(black, 6).

initial_rook_square(white, queenside, Square) :-
  fen:square_codes(Square, "a1").

initial_rook_square(white, kingside, Square) :-
  fen:square_codes(Square, "h1").


initial_rook_square(black, kingside, Square) :-
  fen:square_codes(Square, "h8").

initial_rook_square(black, queenside, Square) :-
  fen:square_codes(Square, "a8").


%%

initial_king_square(white, Square) :-
  fen:square_codes(Square, "e1").

initial_king_square(black, Square) :-
  fen:square_codes(Square, "e8").

%%

castled_rook_square(white, queenside, Square) :-
  fen:square_codes(Square, "d1").

castled_rook_square(white, kingside, Square) :-
  fen:square_codes(Square, "f1").

castled_rook_square(black, kingside, Square) :-
  fen:square_codes(Square, "f8").

castled_rook_square(black, queenside, Square) :-
  fen:square_codes(Square, "d8").


%%

castled_king_square(white, queenside, Square) :-
  fen:square_codes(Square, "c1").

castled_king_square(white, kingside, Square) :-
  fen:square_codes(Square, "g1").

castled_king_square(black, kingside, Square) :-
  fen:square_codes(Square, "g8").

castled_king_square(black, queenside, Square) :-
  fen:square_codes(Square, "c8").
