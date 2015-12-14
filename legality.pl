
:- module(legality, []).

:- use_module(movement).
:- use_module(pgnmove).
:- use_module(square).
:- use_module(position).

legal(P) :-
    king_is_safe(P),
    no_pawns_on_last_rank(P).

no_pawns_on_edge_rank(P) :-
    setof(
        Rank, 
        (
            friendly_pawn(P, PawnSquare),
            square:square_rank(PawnSquare, Rank)
        ),
        Ranks
    ),
    maplist(between(2,7), Ranks).


king_is_safe(P) :- king_attackers(P, []).

king_attackers(P, Attackers) :-
    friendly_king(P, KingSquare),
    setof(
        EnemySquare,
        (
            enemy_piece(P, EnemySquare),
            can_capture(P, EnemySquare, KingSquare)
        ),
        Attackers
    ).

friendly_king(P, Square) :-
    position:turn(P, Turn),
    position:occupant(P, Square, [Turn, king]).

enemy_piece(P, Square) :-
    position:turn(P, Turn),
    position:opposite(Turn, Opposite),
    position:occupant(P, Square, [Opposite, _]).
    
can_capture(P, SourceSquare, TargetSquare) :- 
    position:occupant(P, SourceSquare, [Turn, PieceType]),
    position:occupant(P, SourceSquare, [Opposite, _]),

    position:turn(P, Turn),
    position:opposite(Turn, Opposite),

    can_capture(P, [Turn, PieceType], SourceSquare, TargetSquare).

can_reach(P, knight, Source, Target) :- knights_jump(Source, Target).

can_reach(P, king, Source, Target) :- distance(Source, Target, 2), diagonal(Source, Target).
can_reach(P, king, Source, Target) :- 
    distance(Source, Target, 1), 
    (
        vertical(Source, Target) ; horizontal(Source, Target
    ).

can_reach(P, queen, Source, Target) :- diagonal(Source, Target) ; horizontal(Source, Target) ; vertical(Source, Target).


can_attack_diagonally(P, PieceSquare, TargetSquare) :-
    position:occupant(P, PieceSquare, [Turn, PieceType]),
    member(PieceType, [queen, bishop, king]).
    
