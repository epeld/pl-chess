

% rnbqkbnr/pp1ppppp/8/2p5/4P3/5N2/PPPP1PPP/RNBQKB1R b KQkq - 1 2

example_fen("rnbqkbnr/pp1ppppp/8/2p5/4P3/5N2/PPPP1PPP/RNBQKB1R b KQkq - 1 2").
parsed_board(example_board, FEN) :-
    example_fen(FEN).

example_board(B) :- parsed_board(B, F), example_fen(F).

fen_parts(FEN, Parts) :-
    split(Parts, " ", FEN, 6).

fen_board_rows(FEN, Rows) :-
    fen_parts(FEN, Parts),
    first(Parts, Board),
    split(Rows, "/", Board, 8).

nth_fen_piece([Char | _], 0, Piece) :-
    fen_piece([Char], Piece).

nth_fen_piece([Char | Rest], N, Piece) :-
    fen_piece([Char], _),
    between(1, 8, N),
    plus(Nless, 1, N),
    nth_fen_piece(Rest, Nless, Piece).

nth_fen_piece([Char | FENRow], N, Piece) :-
    number_term([Char], Skip),
    between(Skip, 8, N),
    plus(Nnew, Skip, N),
    nth_fen_piece(FENRow, Nnew, Piece).

nth_fen_piece([Char | _], N, nothing) :-
    number_term([Char], Skip),
    plus(Less, 1, Skip),
    between(0, Less, N).

piece_at([F,R], Board, Piece) :-
    parsed_board(Board, FEN),
    fen_board_rows(FEN, FENRows),
    nth0(R, FENRows, FENRow),
    nth_fen_piece(FENRow, F, Piece).


fen_piece(Char, [PieceType, Color]) :-
    fen_color(Char, Color),
    fen_piece_type(Upper, PieceType),
    term_upper(Char, Upper).

fen_color(Char, white) :- term_upper(Char, Char).
fen_color(Char, black) :- term_lower(Char, Char).

fen_piece_type("P", pawn).
fen_piece_type("B", bishop).
fen_piece_type("R", rook).
fen_piece_type("N", knight).
fen_piece_type("Q", queen).
fen_piece_type("K", king).

:- begin_tests(fen).

test(fen_piece) :-
    fen_piece("R", [rook, white]).

test(fen_rows) :-
    example_fen(FEN),
    fen_parts(FEN, Parts),
    length(Parts, 6).

test(piece_at) :-
    piece_at([1,0], Board, [knight, black]),
    parsed_board(Board, FEN),
    example_fen(FEN).

test(piece_at2) :-
    piece_at([4,3], Board, nothing),
    parsed_board(Board, FEN),
    example_fen(FEN).

:- end_tests(fen).
