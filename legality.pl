
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
    
can_capture(P, PieceSquare, TargetSquare) :- fail.
