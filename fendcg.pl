


% "rnbqkbnr/pp1ppppp/8/2p5/4P3/5N2/PPPP1PPP/RNBQKB1R b KQkq - 1 2".


/*
fen(Position) -->
    board(Board),
    " "
    turn(Turn),
    " "
    rights(Rights),
    " "
    passant(Passant),
    " "
    number(FullMove),
    " "
    number(HalfMove),
    {
        position_parts(Position, [Board, Turn, Rights, Passant, FullMove, HalfMove])
    }.

*/

board(Board) -->
    {
        length(Board, 64), 
        length(Row8, 8),
        length(Row7, 8),
        length(Row6, 8),
        length(Row5, 8),
        length(Row4, 8),
        length(Row3, 8),
        length(Row2, 8),
        length(Row1, 8),
        append([Row1, Row2, Row3, Row4, Row5, Row6, Row7, Row8], Board)
    },
    row(Row8), ['/'],
    row(Row7), ['/'],
    row(Row6), ['/'],
    row(Row5), ['/'],
    row(Row4), ['/'],
    row(Row3), ['/'],
    row(Row2), ['/'],
    row(Row1).




interleaved([Part1 | Parts], Inbetween, List) :- length(Part1, 8), append(Part1, Inbetween, A), append(A, B, List), interleaved(Parts, Inbetween, B).
interleaved([], _, []).

row(Row) --> { length(Row, 8) }, rle_row(Row).


rle_row([Piece | Rest]) --> piece(Piece), rle_row(Rest).
rle_row(Elements) --> 
    {
        member(X, [1,2,3,4,5,6,7,8]), length(Elements, X), 
        append(Nothings, [Piece | Rest], Elements)
    }, 
    nothings(Nothings), 
    piece(Piece), 
    rle_row(Rest). % an intermediate 'empty'-sequence

rle_row(Nothings) --> nothings(Nothings). % the ending 'empty'-sequence
rle_row([]) --> [].


nothings(Nothings) -->
    [N0],
    {
        string_chars("12345678", L), 
        member(N0, L), 
        char_type(N0, digit(Nr)), 
        length(Nothings, Nr), 
        maplist(=(nothing), Nothings) 
    }.


piece(Piece) --> [Piece], { is_piece(Piece) }.


is_piece(Piece) :- string_chars("PNBRQKpnbrqk", Chars), member(Piece, Chars).


:- begin_tests(fen).

test(board_decode, [nondet]) :- string_chars("rnbqkbnr/pp1ppppp/8/2p5/4P3/5N2/PPPP1PPP/RNBQKB1R", L), phrase(board(_), L).
test(board_decode, [nondet]) :- string_chars("8/8/8/8/8/8/8/8", L), phrase(board(_), L).
test(board_encode, [nondet]) :- length(X, 64), maplist(=(nothing), X), phrase(board(X), L), string_chars("8/8/8/8/8/8/8/8", L).

test(row_decode, [nondet]) :- phrase(row(X), ['R', '2', b, n, '3']), X = ['R', nothing, nothing, b, n, nothing, nothing, nothing].
test(row_decode2, [nondet]) :- string_chars("rnbqkbnr", L), phrase(row(L), L). 
test(row_decode3, [nondet]) :- string_chars("pp1ppppp", L), phrase(row([p,p,nothing,p,p,p,p,p]), L). 
test(row_decode4, [nondet]) :- string_chars("8", L), phrase(row(X), L), length(X, 8), maplist(=(nothing), X). 
test(row_decode5, [nondet]) :- string_chars("rrrrrrrr", L), phrase(row([r,r,r,r,r,r,r,r]), L). 
test(row_decode6, [nondet]) :- string_chars("RNBQKB1R", L), phrase(row(X), L), X = ['R', 'N', 'B', 'Q', 'K', 'B', nothing, 'R']. 
test(row_decode7, [nondet]) :- string_chars("5N2", L), phrase(row(_), L). 
test(row_encode, [nondet]) :- phrase(row(['R', nothing, nothing, b, n, nothing, nothing, nothing]), X), X = ['R', '2', b, n, '3'].


test(nothings_decode, [nondet]) :-  phrase(nothings(X), ['3']), X = [nothing, nothing, nothing].
test(nothings_encode, [nondet]) :-  phrase(nothings([nothing, nothing, nothing]), [X]), X = '3'.
test(nothings_encode2, [nondet]) :-  length(X, 8), maplist(=(nothing), X), phrase(nothings(X), L), L = ['8'].


test(piece_decode, [nondet]) :- phrase(piece(X), [r]), X = r.
test(piece_encode, [nondet]) :- phrase(piece('N'), [X]), X = 'N'.

:- end_tests(fen).
