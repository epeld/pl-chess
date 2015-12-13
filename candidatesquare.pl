:- module(candidatesquare, [ candidate_source_square/3 ]).

:- use_module(pgnmove).
:- use_module(position).

% A candidate source square of a Move is a source square
% that is occupied by a candidate piece
candidate_source_square(Move, Pos, Square) :-
    pgnmove:source_square(Move, Square),
    position:board_occupant(Pos, Square, Piece),
    compatible_piece(Move, Pos, Piece).


compatible_piece(Move, Pos, [PieceType, Color]) :-
    pgnmove:moved_piece_type(Move, PieceType),
    position:turn(Pos, Color).
