
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


% TODO handle castling
board_after(Position, FullMove, NewBoard) :-
    (officer_move(FullMove) ; standard_pawn_move(FullMove)),

    pgnmove:source_square(FullMove, Source),
    pgnmove:destination(FullMove, Target),

    position:board(Position, Board),
    position:board_move(Board, Square, Target, NewBoard).

board_after(Position, FullMove, NewBoard) :-
    passant_move(FullMove),

    pgnmove:source_square(FullMove, Source),
    pgnmove:destination(FullMove, Target),

    position:board(Position, Board),
    position:board_put(Board, PassantSquare, nothing, B2),
    position:board_move(B2, Square, Target, NewBoard).

movenumber_after(Position, _, NewNr) :-
    turn(Position, black),
    move_number(Position, Nr),
    next(Nr, NewNr).

movenumber_after(Position, _, Nr) :-
    turn(Position, white),
    move_number(Position, Nr).

half_movenumber_after(_, FullMove, 0) :- pawn_move(FullMove).
half_movenumber_after(_, FullMove, 0) :- capture(FullMove).
half_movenumber_after(Position, FullMove, NewNr) :- 
    half_move_number(Position, Nr),
    NewNr #= Nr + 1.

castling_rights_after(Position, FullMove, NewRights) :-
    castling_rights(Position, NewRights). % TODO implement

passant_after(_, FullMove, PassantSquare) :-
    long_pawn_move(FullMove),
    source_square(FullMove, Source),
    destination(FullMove, Destination),
    (
        below(PassantSquare, Source), below(Destination, PassantSquare) ;
        above(PassantSquare, Source), above(Destination, PassantSquare)
    ).

passant_after(_, FullMove, nothing) :-
    short_pawn_move(FullMove),

    source_square(FullMove, Source),
    destination(FullMove, Destination),
    (
        right_of(Source, Destination) ;
        left_of(Source, Destination) ;
        distance(Source, Target, 1)
    ).

passant_after(_, FullMove, nothing) :- officer_move(FullMove).
passant_after(_, queenside, nothing). % Castling
passant_after(_, kingside, nothing). % Castling

turn_after(Position, _, NewTurn) :- turn(Position, T), opposite(T, NewTurn).

position_after(Position, FullMove, ResultingPosition) :-
    board_after(Position, FullMove, NewBoard),
    turn_after(Position, FullMove, NewTurn),
    castling_rights_after(Position, FullMove, NewRights),
    passant_after(Position, FullMove, NewPassant),
    move_number_after(Position, FullMove, NewNr),
    half_move_number_after(Position, FullMove, NewHalfNr),

    position:parts(
        ResultingPosition, 
        [NewBoard, NewTurn, NewRights, NewPassant, NewHalfNr, NewNr]
    ).


standard_pawn_move(Move) :-
    pawn_move(Move),

    non_passant_squares(Position, NonPassant),
    destination(Move, Destination),
    member(Destination, NonPassant).

non_passant_squares(Position, Squares) :-
    passant_square(Position, Passant),
    bag_of(Square, square(Square), All),
    delete(All, Passant, Squares).

passant_move(Move, PassantSquare) :-
    pawn_move(Move),

    destination(Move, Destination), source_square(Move, Source),

    passant_square(Source, Destination, PassantSquare).


valid_move(Position, FullMove) :- 
    pgnmove:source_square(FullMove, Source),
    pgnmove:destination(FullMove, Target),
    pgnmove:moved_piece_type(PieceType),
    pgnmove:move_type(MoveType),

    position:turn(Turn),
    Piece = [Turn, PieceType],

    movement:piece_can(MoveType, Piece, Source, Target, IntermediateSquares),
    all_empty(Position, IntermediateSquares),

    legal_result(Position, FullMove).

all_empty(Position, Squares) :-
    apply:maplist(square_is_empty(Position), Squares).

