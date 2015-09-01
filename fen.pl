
:- use_module(listutils).
:- use_module(square).

% rnbqkbnr/pp1ppppp/8/2p5/4P3/5N2/PPPP1PPP/RNBQKB1R b KQkq - 1 2

example_fen("rnbqkbnr/pp1ppppp/8/2p5/4P3/5N2/PPPP1PPP/RNBQKB1R b KQkq - 1 2").


fen_string(FEN, Position) :- string(FEN), string_chars(FEN, Chars), fen_string(Chars, Position).

fen_string(Chars, Position) :-
    compound(Chars),
    fen_parts(Chars, Parts),
    Parsers = [fen_board, fen_turn, fen_castling, fen_passant, fen_movenumber, fen_movenumber],
    maplist(call, Parsers, Parts, Position).
    

fen_castling(['-'], []).

fen_castling(A0, B0) :- string(A0), string_chars(A0, A0Chars), fen_castling(A0Chars, B0).

fen_castling(A0, B0) :- 
    compound(A0), length(A0, N), N > 0,
    string_chars("KQkq", KQkq), listutils:sublist(KQkq, A0),
    maplist(fen_castling_right, A0, B0).


fen_castling_right(Char, [Color, Piece]) :- member(Piece, [king, queen]), fen_piecetype(Char, Piece), fen_color(Char, Color).


fen_board(BoardPart, Board) :-
    fen_board_rows(BoardPart, Rows),
    reverse(ReversedRows, Rows),
    maplist(fen_single_row, ReversedRows, ParsedRows),
    append(ParsedRows, Board).


fen_turn('w', white).
fen_turn("w", white).
fen_turn('b', black).
fen_turn("b", black).


fen_passant('-', nothing).
fen_passant("-", nothing).
fen_passant(SquareChars, Square) :- square_chars(Square, SquareChars).
fen_passant(SquareString, Square) :- square_string(Square, SquareString).


fen_movenumber(Number, Term) :- parsed_number(Term, Number).


fen_parts(FEN, Parts) :-
    length(Parts, 6),
    split_chars(FEN, ' ', Parts).


fen_board_rows(BoardPart, Rows) :-
    length(Rows, 8),
    split_chars(BoardPart, '/', Rows).


fen_single_row(A, B) :- fen_single_row(A, B, 8).

fen_single_row(_, [], 0).

fen_single_row([Char | Rest], [Piece | Row], N0) :-
    fen_piece(Char, Piece),
    N is N0 - 1,
    fen_single_row(Rest, Row, N).

fen_single_row([NumberChar | RestChars], Row, N0) :-
    char_type(NumberChar, digit(M)), between(1, N0, M), filled(nothing, Empties, M),
    N is N0 - M,
    append(Empties, Rest, Row),
    fen_single_row(RestChars, Rest, N).


fen_piece(Char, [PieceType, Color]) :- fen_color(Char, Color), fen_piecetype(Char, PieceType).


fen_color(Char, white) :- fen_piecetype(Char, _), char_type(Char, upper).
fen_color(Char, black) :- fen_piecetype(Char, _), char_type(Char, lower).


fen_piecetype(p, pawn).
fen_piecetype(b, bishop).
fen_piecetype(r, rook).
fen_piecetype(n, knight).
fen_piecetype(q, queen).
fen_piecetype(k, king).
fen_piecetype(Char, Piece) :- char_type(Char, upper(Lower)), fen_piecetype(Lower, Piece).


:- begin_tests(fen).

test(fen_piecetype, [nondet]) :- 
    fen_piecetype('R', rook),
    fen_piecetype('N', knight),
    fen_piecetype('Q', queen).

test(fen_passant, [nondet]) :- fen_passant("e4", Square), square_file(Square, 'e'), square_rank(Square, 4).
test(fen_castling, [nondet]) :- fen_castling("KQk", [[white, king], [white, queen], [black, king]]).


:- end_tests(fen).
