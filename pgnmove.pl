

use_module(move).


move(Move) --> pawn_move(Move).
move(Move) --> piece_move(Move).
move(Move) --> castling_move(Move).


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


piece_move(PieceMove) -->
    officer(Officer),
    source_indicator(Source),
    move_type(MoveType),
    square(Destination),
    { move(PieceMove, [Officer, Source, MoveType, Destination]).


castling_move(CastlingMove) --> ['O-O-O'], { move(CastlingMove, [castling, queenside]) }.
castling_move(CastlingMove) --> ['O-O'], { move(CastlingMove, [castling, kingside]) }.


move_type(capture) --> ['x'].
move_type(move) --> [].


source_indicator(nothing) --> [].
source_indicator(File) --> file(File).
source_indicator(Rank) --> rank(Rank).
source_indicator(Square) --> square(Square).


file(File) -->
    [File],
    { square:file(_, File) }.


rank(RankChar) -->
    [RankChar],
    { square:rank_char(_, RankChar) }.


square(Square) -->
    file(File), rank(Rank),
    { square_chars(Square, [File, Rank]) }.


officer(Officer) --> [OfficerChar], { officer_char(Officer, OfficerChar) }.


promotion(nothing) --> [].
promotion(Promotee) --> ['='], promotee(Promotee).


promotee(Promotee) --> [Atom], { promotee_atom(Promotee, Atom) }.


promotee_atom(bishop, 'B').
promotee_atom(knight, 'N').
promotee_atom(rook, 'R').
promotee_atom(queen, 'Q').


officer_char(Officer, Atom) :- promotee_atom(Officer, Atom).
officer_char(king, 'K').
