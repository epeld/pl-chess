


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

board(Board) -->
    
    row, "/",

    row, "/",

    row, "/",

    row, "/",

    row, "/",

    row, "/",

    row, "/",

    row, "/",

    row, "/",
    
    .


*/

row(Row) --> { length(Row, 8) }, rle_row(Row).


rle_row([Piece | Rest]) --> piece(Piece), rle_row(Rest).
rle_row(Elements) --> { append(Nothings, [Piece | Rest], Elements) }, nothings(Nothings), piece(Piece), rle_row(Rest). % an intermediate 'empty'-sequence
rle_row(Nothings) --> nothings(Nothings). % the ending 'empty'-sequence
rle_row([]) --> [].

nothings(Nothings) -->
    [N0],
    {   member(N0, [1,2,3,4,5,6,7,8]), length(Nothings, N0), maplist(=(nothing), Nothings)  }.


piece(Piece) --> [Piece], { is_piece(Piece) }.


is_piece(Piece) :- string_chars("NBRQKnbrqk", Chars), member(Piece, Chars).

:- begin_tests(fen).

test(row_decode, [nondet]) :- phrase(row(X), ['R', 2, b, n, 3]), X = ['R', nothing, nothing, b, n, nothing, nothing, nothing].
test(row_encode, [nondet]) :- phrase(row(['R', nothing, nothing, b, n, nothing, nothing, nothing]), X), X = ['R', 2, b, n, 3].


test(nothings_decode, [nondet]) :-  phrase(nothings(X), [3]), X = [nothing, nothing, nothing].
test(nothings_encode, [nondet]) :-  phrase(nothings([nothing, nothing, nothing]), [X]), X = 3.

:- end_tests(fen).
