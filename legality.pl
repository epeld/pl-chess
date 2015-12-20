
:- module(legality, []).

:- use_module(movement).
:- use_module(pgnmove).
:- use_module(square).
:- use_module(position).
:- use_library(apply).

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
    

square_is_empty(Position, Square) :-
    position:occupant(Position, Square, nothing).


legal_result(Position, FullMove) :-
    position_after(Position, FullMove, ResultingPosition),
    legal(ResultingPosition).

position_after(Position, FullMove, ResultingPosition) :-
    pgnmove:source_square(FullMove, Source),
    pgnmove:destination(FullMove, Target),

    % TODO define this predicate
    position:move(Position, Source, Target).

% TODO generalize to avoid copy paste for reach/capture

all_empty(Position, Squares) :-
    apply:maplist(square_is_empty(Position), Squares).

piece_can_reach(Position, FullMove) :- 
    pgnmove:source_square(FullMove, Source),
    pgnmove:destination(FullMove, Target),
    pgnmove:moved_piece_type(PieceType),
    pgnmove:move_type(MoveType),

    position:turn(Turn),
    Piece = [Turn, PieceType],

    % TODO move this out of legality and into position:move?
    movement:piece_can_reach(Piece, Source, Target, IntermediateSquares),
    all_empty(Position, IntermediateSquares),

    legal_result(Position, FullMove).


piece_can_capture(Position, Piece, Source, Target) :- 
    pgnmove:source_square(FullMove, Source),
    pgnmove:destination(FullMove, Target),
    pgnmove:moved_piece_type(PieceType),

    position:turn(Turn),
    Piece = [Turn, PieceType],

    movement:piece_can_capture(Piece, Source, Target, IntermediateSquares),
    apply:maplist(square_is_empty(Position), IntermediateSquares),

    legal_result(Position, FullMove).
