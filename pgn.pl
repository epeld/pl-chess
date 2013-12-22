
pgn_rank(R, RTerm) :-
    number_term(RTerm, Rnew),
    plus(R, 1, Rnew).

pgn_file(0, "a").
pgn_file(1, "b").
pgn_file(2, "c").
pgn_file(3, "d").
pgn_file(4, "e").
pgn_file(5, "f").
pgn_file(6, "g").
pgn_file(7, "h").


pgn_square([F, R], [FTerm, RTerm]) :-
    pgn_rank(R, [RTerm]),
    pgn_file(F, [FTerm]).

pgn_short_pawnmove(Move, Term) :-
    pgn_square(Destination, Term),
    move_destination(Move, Destination),
    move_type(Move, moves),
    move_source_indicator(Move, nothing),
    move_piecetype(Move, pawn).

% TODO MetaInfo are not quite working yet

pgn_movetype(takes, "x").
pgn_movetype(moves, "").

pgn_source_indicator(Indicator, Term) :-
    pgn_rank(R, Term),
    source_indicator(Indicator, rank, R).

pgn_source_indicator(Indicator, Term) :-
    pgn_file(F, Term),
    source_indicator(Indicator, file, F).

pgn_source_indicator(Indicator, Term) :-
    pgn_square(S, Term),
    source_indicator(Indicator, square, S).

pgn_check(Term) :- term_member("+", Term).
pgn_check(Term) :- term_member("#", Term).
pgn_mate(Term) :- term_member("#", Term).

pgn_meta(nothing, _).

pgn_long_pawnmove(Move, Term) :-
    append([SourceIndicator, MoveType, DestinationSquare, MetaInfo], Term),
    pgn_source_indicator(Indicator, SourceIndicator),
    pgn_movetype(Type, MoveType),
    pgn_square(Destination, DestinationSquare),
    pgn_meta(Meta, MetaInfo),
    move(Move, [pawn, Indicator, Type, Destination, Meta]).

pgn_pawnmove(Move, Term) :- pgn_long_pawnmove(Move, Term).
pgn_pawnmove(Move, Term) :- pgn_short_pawnmove(Move, Term).

pgn_castles([castles, kingside], "O-O").
pgn_castles([castles, queenside], "O-O-O").

pgn_maybe(Goal, Parsed, Term) :-
    call(Goal, Parsed, Term).

pgn_maybe(_, nothing, "").

pgn_piecemove(Move, Term) :-
    append([PieceType, MaybeSource, MoveType, DestinationSquare, MetaInfo], Term),
    fen_piecetype(PieceType, PT),
    pgn_maybe(pgn_source_indicator, Source, MaybeSource),
    pgn_movetype(MT, MoveType),
    pgn_square(Destination, DestinationSquare),
    pgn_meta(Meta, MetaInfo),
    move(Move, [PT, Source, MT, Destination, Meta]).

pgn_move(Move, Term) :- pgn_pawnmove(Move, Term).
pgn_move(Move, Term) :- pgn_piecemove(Move, Term).
pgn_move(Move, Term) :- pgn_castles(Move, Term).
