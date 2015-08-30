

use_module(move).



pawn_move(PawnMove) -->
    square(Destination),
    promotion(Promotion),
    { move(PawnMove, [pawn, nothing, move, Destination, Promotion]) }.

pawn_move(PawnMove) -->
    file(SourceFile),
    ['x'],
    square(Destination),
    promotion(Promotion),
    { move(PawnMove, [pawn, SourceFile, capture, Destination, Promotion]) }.


edge_rank('1').
edge_rank('8').


file(File) -->
    [File],
    { square:file(_, File) }.


rank(RankChar) -->
    [RankChar],
    { square:rank_char(_, RankChar) }.


square(Square) -->
    file(File), rank(Rank),
    { square_chars(Square, [File, Rank]) }.


promotion(nothing) --> [].
promotion(Officer) --> ['='], officer(Officer).


officer(Officer) --> [Atom], { officer_atom(Officer, Atom) }.

officer_atom(bishop, 'B').
officer_atom(knight, 'N').
officer_atom(rook, 'R').
officer_atom(queen, 'Q').
