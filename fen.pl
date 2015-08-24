
use_module(listutils).

% rnbqkbnr/pp1ppppp/8/2p5/4P3/5N2/PPPP1PPP/RNBQKB1R b KQkq - 1 2

example_fen("rnbqkbnr/pp1ppppp/8/2p5/4P3/5N2/PPPP1PPP/RNBQKB1R b KQkq - 1 2").
parsed_board(example_board, FEN) :-
    example_fen(FEN).

example_board(B) :- parsed_board(B, F), example_fen(F).

fen_string(FEN, Position) :-
    fen_parts(FEN, Position),
    Parsers = [fen_board, fen_turn, fen_castling, fen_passant, fen_movenumber, fen_movenumber],
    maplist(call, Parsers, Position).
    
fen_string2(FEN, Position) :-
    fen_parts(FEN, [BoardPart, TurnPart, CastlingPart, PassantPart,
		    FullMovePart, HalfMovePart]),
    fen_board(BoardPart, Board),
    fen_turn(TurnPart, Turn),
    fen_castling(CastlingPart, Castling),
    fen_passant(Passant, PassantPart),
    fen_movenumber(FullMove, FullMovePart),
    fen_movenumber(HalfMove, HalfMovePart),
    Position = [Board, Turn, Castling, Passant, FullMove, HalfMove].


fen_castling(S, []) :- string_chars("-", S).
fen_castling(A0, B0) :- 
    length(A0, N), N > 0,
    string_chars("KQkq", KQkq), listutils:sublist(KQkq, A0),
    maplist(fen_castling_right, A0, B0).


fen_castling_right(Char, [Color, Piece]) :- member(Piece, [king, queen]), fen_piecetype(Char, Piece), fen_color(Char, Color).


fen_board(BoardPart, Board) :-
    fen_board_rows(BoardPart, ReversedRows),
    reverse(ReversedRows, Rows),
    maplist(fen_single_row, Rows, ParsedRows),
    board_rows(Board, ParsedRows).

fen_turn("w", white).
fen_turn("b", black).

fen_passant("-", nothing).
fen_passant(Term, Square) :- pgn_square(Square, Term).

fen_movenumber(Number, Term) :- parsed_number(Term, Number).

fen_parts(FEN, Parts) :-
    split_string(FEN, " ", "", Parts),
    length(Parts, 6).

fen_board_rows(BoardPart, Rows) :-
    split_string(BoardPart, "/", "", Rows),
    length(Rows, 8).


fen_single_row(A, B) :- fen_single_row(A, B, 8).

fen_single_row(_, [], 0).

fen_single_row([Char | Rest], [Piece | Row], N0) :-
    fen_piece([Char], Piece),
    N is N0 - 1,
    fen_single_row(Rest, Row, N).

fen_single_row([NumberChar | RestChars], Row, N0) :-
    number_term([NumberChar], M), between(1, 8, M), filled(nothing, Empties, M),
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
