

rank(Coord, Char) :- char_type(Char, digit(R)), between(0, 7, Coord), R is Coord + 1.


file(0, 'a').
file(1, 'b').
file(2, 'c').
file(3, 'd').
file(4, 'e').
file(5, 'f').
file(6, 'g').
file(7, 'h').


square_file([File, _], File).
square_rank([_, Rank], Rank).


square([FileChar, RankChar], [File, Rank]) :- file(File, FileChar), rank(Rank, RankChar).
