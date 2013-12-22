

% rnbqkbnr/pp1ppppp/8/2p5/4P3/5N2/PPPP1PPP/RNBQKB1R b KQkq - 1 2

example_fen("rnbqkbnr/pp1ppppp/8/2p5/4P3/5N2/PPPP1PPP/RNBQKB1R b KQkq - 1 2").
parsed_board(example_board, FEN) :-
    example_fen(FEN).

example_board(B) :- parsed_board(B, F), example_fen(F).

fen_string(FEN, Position) :-
    fen_parts(FEN, [BoardPart, TurnPart, CastlingPart, PassantPart,
		    FullMovePart, HalfMovePart]),
    fen_board(BoardPart, Board),
    fen_turn(TurnPart, Turn),
    fen_castling(CastlingPart, Castling),
    fen_passant(Passant, PassantPart),
    fen_movenumber(FullMove, FullMovePart),
    fen_movenumber(HalfMove, HalfMovePart),
    position(Position, [Board, Turn, Castling, Passant, FullMove, HalfMove]).

fen_castling(CastlingPart, Castling) :-
    is_set(CastlingPart),
    every(fen_castling_char, CastlingPart, Castling).

fen_castling_char(Term, Out) :- fen_castling_right([Term], Out).
fen_castling_right("K", [white, kingside]).
fen_castling_right("Q", [white, queenside]).
fen_castling_right("k", [black, kingside]).
fen_castling_right("q", [black, queenside]).


fen_board(BoardPart, Board) :-
    fen_board_rows(BoardPart, ReversedRows),
    reverse(ReversedRows, Rows),
    every(fen_single_row, Rows, ParsedRows),
    every(length8, ParsedRows),
    board_rows(Board, ParsedRows).

length8(L) :- length(L, 8).

fen_turn("w", white).
fen_turn("b", black).

fen_passant(nothing, "-").
fen_passant(Square, Term) :- pgn_square(Square, Term).

fen_movenumber(Number, Term) :- parsed_number(Term, Number).

position(X,X). % abstraction
position_board(Pos, Board) :- nth0(0, Pos, Board).
board_rows(X, X).


fen_parts(FEN, Parts) :-
    split(Parts, " ", FEN, 6).

fen_board_rows(BoardPart, Rows) :-
    split(Rows, "/", BoardPart, 8).

fen_single_row([], []).
fen_single_row([Char | Rest], [Piece | Row]) :-
    fen_piece([Char], Piece),
    fen_single_row(Rest, Row).

fen_single_row([NumberChar | RestChars], Row) :-
    number_term([NumberChar], N),
    between(1, 8, N),
    length(Empties, N),
    every(is_empty, Empties),
    fen_single_row(RestChars, Rest),
    append(Empties, Rest, Row).

is_empty(nothing).

fen_piece(Char, [PieceType, Color]) :-
    fen_color(Char, Color),
    fen_piecetype(Upper, PieceType),
    term_upper(Char, Upper).

fen_color(Char, white) :- term_upper(Char, Char).
fen_color(Char, black) :- term_lower(Char, Char).

fen_piecetype("P", pawn).
fen_piecetype("B", bishop).
fen_piecetype("R", rook).
fen_piecetype("N", knight).
fen_piecetype("Q", queen).
fen_piecetype("K", king).


piece_at([F,R], Position, Piece) :-
    position_board(Position, Board),
    board_rows(Board, Rows),
    nth0(R, Rows, Row),
    nth0(F, Row, Piece).

:- begin_tests(fen).

test(fen_piece) :-
    fen_piece("R", [rook, white]).

test(fen_rows) :-
    example_fen(FEN),
    fen_parts(FEN, Parts),
    length(Parts, 6).

test(piece_at) :-
    example_fen(FEN),
    fen_string(FEN, Pos),
    atom_square(S, g8),
    piece_at(S, Pos, [knight, black]).


test(piece_at2) :-
    example_fen(FEN),
    fen_string(FEN, Pos),
    atom_square(S, e5),
    piece_at(S, Pos, nothing).

:- end_tests(fen).
