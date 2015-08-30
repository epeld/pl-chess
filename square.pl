
use_module(library(clfpd)).


edge_rank('1').
edge_rank('8').


rank(Coord, Rank) :- between(0, 7, Coord), Rank is Coord + 1.

rank_char(Rank, Char) :- rank(_, Rank), char_type(Char, digit(Rank)).


file(0, 'a').
file(1, 'b').
file(2, 'c').
file(3, 'd').
file(4, 'e').
file(5, 'f').
file(6, 'g').
file(7, 'h').


files(X) :- string_chars("abcdefgh", X).


square_file_coord(Sq, Coord) :- square_file(Sq, File), file(Coord, File).
square_rank_coord(Sq, Coord) :- square_rank(Sq, Rank), rank(Coord, Rank).


square_file([File, _], File).
square_rank([_, Rank], Rank).


square_index(Square, Index) :-
    between(0, 63, Index),
    square([FileCoord, RankCoord], Square),
    Index #= RankCoord * 8 + FileCoord.


square_chars([File, Rank], [File, RankChar]) :- 
    files(Files), 
    member(File, Files), 
    char_type(RankChar, digit(Rank)).


square_string(Sq, S) :- string(S), string_chars(S, Chars), square_chars(Sq, Chars).


square([CoordX, CoordY], [File, Rank]) :- file(CoordX, File), rank(CoordY, Rank).


:- begin_tests(square).

test(square, [nondet]) :- square([5, 2], ['f', 3]).

:- end_tests(square).
