:- module(pgnmovedcg).


:- use_module(pgnmove).
:- use_module(square).


move(Move) --> pawn_move(Move).
move(Move) --> piece_move(Move).
move(Move) --> castling_move(Move).


pawn_move(PawnMove) -->
    square(Destination),
    promotion(Promotion),
    {
        pgnmove:pawn_move(Move),
        pgnmove:source_indicator(Move, nothing),
        pgnmove:move_type(Move, moves),
        pgnmove:destination(Move, Destination)
        pgnmove:promotion(Move, Promotion)
    }.

pawn_move(PawnMove) -->
    file(SourceFile),
    ['x'],
    square(Destination),
    promotion(Promotion),
    {
        pgnmove:pawn_move(Move),
        pgnmove:source_indicator(Move, Source),
        pgnmove:move_type(Move, captures),
        pgnmove:destination(Move, Destination)
        pgnmove:promotion(Move, Promotion)
    }.


piece_move(PieceMove) -->
    officer(Officer),
    source_indicator(Source),
    move_type(MoveType),
    square(Destination),
    {
        pgnmove:officer_move(Move),
        pgnmove:moved_officer(Move, Officer),
        pgnmove:source_indicator(Move, Source),
        pgnmove:move_type(Move, MoveType),
        pgnmove:destination(Move, Destination)
    }.


castling_move(queenside) --> ['O-O-O'].
castling_move(kingside) --> ['O-O'].


move_type(capture) --> ['x'].
move_type(move) --> [].


source_indicator(nothing) --> [].
source_indicator(File) --> file(File).
source_indicator(Rank) --> rank(Rank).
source_indicator(Square) --> square(Square).


file(File) --> [File],
    {
        square:file(File)
    }.


rank(Rank) --> [RankChar],
    {
        square:rank_char(Rank, RankChar)
    }.


square(Square) --> file(File), rank(Rank),
    {
        square:square_file(Square, File),
        square:square_rank(Square, Rank)
    }.


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
