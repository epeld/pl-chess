:- module(fen, []).

string(Position, String) :-
  phrase(position(Position), String).

initial_position(X) :- initial_fen_string(Fen), string(X, Fen).

initial_fen_string("rnbqkbnr/pp1ppppp/8/2p5/4P3/5N2/PPPP1PPP/RNBQKB1R b KQkq - 1 2").

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


turn([position, _, Turn | _], Turn).
passant([position, _, _, Passant | _], Passant).
rights([position, _ , _, _, Rights | _], Rights).
half_move([position, _ , _, _, _, HalfMoveNr | _], HalfMoveNr).
full_move([position, _ , _, _, _, _, FullMoveNr | _], FullMoveNr).

piece_at([position, B | _], Square, Piece) :-
  piece_at(B, Square, Piece).


piece_at([board, Rows], [square, X, Y], Piece) :-
  movement:square(X, Y),

  % Flip the y-coord becaues FEN stores 'black' rows first
  plus(Y0, Y, 7),
  nth0(Y0, Rows, Row),
  nth0(X, Row, Piece).


space --> " ".
    

digits([Digit | Digits]) -->
  digit(Digit),
  digits(Digits).

digits([Digit]) -->
  digit(Digit).


digit(Digit) -->
  {
    member(Digit, "123456789")
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

square([square, X, Y]) -->
  file(X), rank(Y).


rank(Y, [Char | Rest], Rest) :-
  nth0(Y, "12345678", Char).

file(X, [Char | Rest], Rest) :-
  nth0(X, "abcdefgh", Char).
  
  


castling_rights(Rights) -->
  {
    pick("KQkq", Chars),
    maplist(castling_right, Rights, Chars)
  },
  Chars.

pick([X | Xs], [X | Ys]) :-
  pick(Xs, Ys).

pick([_ | Xs], Ys) :-
  pick(Xs, Ys).

pick([], []).



castling_right([Side, white], Char) :-
  nth0(Ix, "KQ", Char),
  nth0(Ix, [king, queen], Side).

castling_right([Side, black], Char) :-
  nth0(Ix, "kq", Char),
  nth0(Ix, [king, queen], Side).


turn(white) --> "w".
turn(black) --> "b".


board([board, Rows]) --> rows(Rows).


rows(Rows) -->
  {
    length(Rows, 8)
  },
  rows_n(Rows).

rows_n([Row | Rows]) -->
  row(Row), "/",
  rows_n(Rows).

rows_n([Row]) -->
  row(Row).


group([Piece | Rest], [[Piece] | Rest2]) :-
  Piece = [_, _],
  group(Rest, Rest2).


group([nothing | Rest], [Nothings | Rest2]) :-
  append(Nothings, [Something | Other], [nothing | Rest]),
  
  maplist(=(nothing), Nothings),
  Something = [_, _],
  
  group([Something | Other], Rest2).


group(Nothings, [Nothings]) :-
  Nothings = [_ | _],
  maplist(=(nothing), Nothings).


group([], []).


row(Row, Before, After) :-
  (var(Row), rle_pieces(Grouped, 8, Before, After) ; nonvar(Row)),
  
  length(Row, 8),
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


piece_char([PieceType, white], Char) :-
  nth0(Ix, "PBNRQK", Char),
  nth0(Ix, [pawn, bishop, knight, rook, queen, king], PieceType).

piece_char([PieceType, black], Char) :-
  nth0(Ix, "pbnrqk", Char),
  nth0(Ix, [pawn, bishop, knight, rook, queen, king], PieceType).

