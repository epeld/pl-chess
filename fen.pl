:- module(fen, [string/2]).

:- set_prolog_flag(double_quotes, codes).

string(Position, String) :-
  once(
    phrase(position(Position), String)
  ).

initial_position(X) :- initial_fen_string(Fen), string(X, Fen).
second_position(X) :- second_fen_string(Fen), string(X, Fen).

%initial_fen_string("rnbqkbnr/pp1ppppp/8/2p5/4P3/5N2/PPPP1PPP/RNBQKB1R b KQkq - 1 2").
initial_fen_string("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1").

second_fen_string("rnbqkbnr/pp1ppppp/8/2p5/4P3/5N2/PPPP1PPP/RNBQKB1R b KQkq - 1 2").

position([position, Board, Turn, Rights, Passant, HalfMoveNr, FullMoveNr]) -->
    board(Board), 
    space,
    turn(Turn), 
    space,
    castling_rights(Rights),
    space,
    passant_square(Passant),
    space,
    nat(HalfMoveNr),
    space,
    nat(FullMoveNr).


board([position, Board | _], Board).
turn([position, _, Turn | _], Turn).
passant([position, _, _, Passant | _], Passant).
rights([position, _ , _, _, Rights | _], Rights).
half_move([position, _ , _, _, _, HalfMoveNr | _], HalfMoveNr).
full_move([position, _ , _, _, _, _, FullMoveNr | _], FullMoveNr).

piece_at([position, B | _], Square, Piece) :-
  piece_at_rows(B, Square, Piece).

rows_row(Rows, Y, Row) :-
  arg(Y, Rows, Row).

row_piece(Row, X, Piece) :-
  arg(X, Row, Piece).

piece_at_rows(Rows, square(X, Y), Piece) :-
  rows_row(Rows, Y, Row),
  row_piece(Row, X, Piece).

space --> " ".
    

digits([Digit | Digits]) -->
  digit(Digit),
  digits(Digits).

digits([Digit]) -->
  digit(Digit).


digit(Digit) -->
  {
    member(Digit, "1234567890")
  },
  [Digit].

nat(N, Before, After) :-
  nonvar(N), !, 
  number_codes(N, Codes),
  digits(Codes, Before, After).


nat(N, Before, After) :-
  var(N), 
  digits(Codes, Before, After),
  number_codes(N, Codes).

passant_square(nothing) --> "-".
passant_square(Square) --> square(Square).


square_codes(Square, Codes) :-
  square(Square, Codes, []).

square(square(X, Y)) -->
  file(X), rank(Y).


rank(Y, [Char | Rest], Rest) :-
  nth1(Y, "12345678", Char).

file(X, [Char | Rest], Rest) :-
  nth1(X, "abcdefgh", Char).
  
  

castling_rights([]) --> "-".

castling_rights(Rs) -->
  castling_rights1(Rs).

castling_rights1([R | Rights]) -->
  { castling_right(R, C) },
  [C],
  castling_rights1(Rights).

castling_rights1([]) --> [].



castling_right(castling_right(kingside, white), 75 /* K */).
castling_right(castling_right(kingside, black), 107 /* k */).
castling_right(castling_right(queenside, white), 81 /* Q */).
castling_right(castling_right(queenside, black), 113 /* q */).


turn(white) --> "w".
turn(black) --> "b".


board(Rows) --> rows(Rows).


rows(rows(R1, R2, R3, R4, R5, R6, R7, R8)) -->
  row(R8),
  "/",
  row(R7),
  "/",
  row(R6),
  "/",
  row(R5),
  "/",
  row(R4),
  "/",
  row(R3),
  "/",
  row(R2),
  "/",
  row(R1).
  

group([piece(Pt,C) | Rest], [[piece(Pt, C)] | Rest2]) :-
  group(Rest, Rest2).


group([nothing | Rest], [Nothings | Rest2]) :-
  append(Nothings, [piece(Pt, C) | Other], [nothing | Rest]),
  
  maplist(=(nothing), Nothings),
  
  group([piece(Pt, C) | Other], Rest2).


group(Nothings, [Nothings]) :-
  Nothings = [_ | _],
  maplist(=(nothing), Nothings).


group([], []).


row(row(P1,P2,P3,P4,P5,P6,P7,P8)) -->
  row_pieces([P1,P2,P3,P4,P5,P6,P7,P8]).

row_pieces(Row, Before, After) :-
  ( ground(Row), !
  ; rle_pieces(Grouped, 8, Before, After) ),
  
  group(Row, Grouped),
  rle_pieces(Grouped, 8, Before, After).

rle_pieces([ Piece | Pieces ], N) -->
  rle_piece(Piece, N, N1),
  rle_pieces(Pieces, N1).

rle_pieces([], 0) --> [].


rle_piece([Piece], N, N1) -->
  piece(Piece), 
  {
    N > 0,
    succ(N1, N)
  }.

rle_piece(Elements, N, N1) -->
  digit(LenDigit), 
  {
    between(1, 8, Len),
    length(Elements, Len),
    maplist(=(nothing), Elements),
    number_codes(Len, [LenDigit]),
    plus(N1, Len, N),
    N1 >= 0
  }.


piece(Piece) -->
  {
    piece_char(Piece, Char)
  },
  [Char].


piece_char(piece(PieceType, white), Char) :-
  nth0(Ix, "PBNRQK", Char),
  nth0(Ix, [pawn, bishop, knight, rook, queen, king], PieceType).

piece_char(piece(PieceType, black), Char) :-
  nth0(Ix, "pbnrqk", Char),
  nth0(Ix, [pawn, bishop, knight, rook, queen, king], PieceType).
