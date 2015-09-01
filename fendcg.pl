:- module(fen, [position//1]).

:- use_module(listutils).
:- use_module(square).
:- use_module(library(clpfd)).
:- use_module(position).


% "rnbqkbnr/pp1ppppp/8/2p5/4P3/5N2/PPPP1PPP/RNBQKB1R b KQkq - 1 2".

position(Position) -->
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
    nat(FullMoveNr),
    position_parts(Position, [Board, Turn, Rights, Passant, HalfMoveNr, FullMoveNr]).


space --> [' '].


digit(0) --> ['0'].
digit(N) --> nonzero(N).


nonzero(1) --> ['1'].
nonzero(2) --> ['2'].
nonzero(3) --> ['3'].
nonzero(4) --> ['4'].
nonzero(5) --> ['5'].
nonzero(6) --> ['6'].
nonzero(7) --> ['7'].
nonzero(8) --> ['8'].
nonzero(9) --> ['9'].



multiples_of_ten(Mults, X) :-
    X0 is X - 1,
    numlist(0, X0, Range),
    reverse(Rev, Range),
    maplist(pow(10), Rev, Mults).


nat(N)   --> 
    {
        between(1, 4, X),
        length(Digits, X),
        Digits = [First | Rest]
    },
    nonzero(First),
    nats(Rest),
    {
        multiples_of_ten(Mults, X),
        scalar_product(Mults, Digits, #=, N)
    }.

nats([D | Digits]) --> digit(D), nats(Digits).
nats([]) --> [].

passant_square(nothing) --> ['-'].
passant_square(Square) --> { square:square_chars(Square, [File, Rank]) }, [File], [Rank].


castling_rights(Rights) -->
    { string_chars("KQkq", AllRights), listutils:sublist(AllRights, Rights) },
    Rights.


turn(white) --> [w].
turn(black) --> [b].


board(Board) --> {  board_occupants(Board, Occupants) }, rows(Occupants).


rows(Rows) -->
        { length(Rs, 8),
          maplist(same_length(Rs), Rs),
          append(Rs, Rows),
          reverse(Rs, [Row8|RRs]) },
        row(Row8),
        rows_(RRs).

rows_([]) --> [].
rows_([R|Rs]) --> [/], row(R), rows_(Rs).



row(Row) --> { length(Row, 8) }, rle_row(Row).


rle_row([Piece | Rest]) --> piece(Piece), rle_row(Rest).
rle_row(Elements) --> 
    {
        member(X, [1,2,3,4,5,6,7,8]), length(Elements, X), 
        append(Nothings, [Piece | Rest], Elements)
    }, 
    nothings(Nothings),  % an intermediate 'empty'-sequence
    piece(Piece), 
    rle_row(Rest).

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

test(rows_decode, [nondet]) :- string_chars("rnbqkbnr/pp1ppppp/8/2p5/4P3/5N2/PPPP1PPP/RNBQKB1R", L), phrase(board(_), L).
test(rows_decode, [nondet]) :- string_chars("8/8/8/8/8/8/8/8", L), phrase(board(_), L).
test(rows_encode, [nondet]) :- length(X, 64), maplist(=(nothing), X), phrase(board(X), L), string_chars("8/8/8/8/8/8/8/8", L).

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
