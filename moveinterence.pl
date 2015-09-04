
:- module(moveinference, [
        uniquely_determined/2
    ]).

:- use_module(move).
:- use_module(movement).
:- use_module(square).
:- use_module(listutils).


% A move is uniquely determined in a given position
% whenever there is only one possible source square
% that it can have
uniquely_determined(Move, Position) :-
    bagof(X, possible_source_square(Move, Position, X), [_]).


legal_source_square(Move, Pos, Square) :-
    full_move(Move, FullMove, Square),
    legal(FullMove, Pos).


% FullMove is an 'Upgrade' of Move with source_indicator set to Square
full_move(Move, FullMove, Square) :-
    piece_move(Move),
    possible_source_square(Move, Pos, Square),
    replacement_at(1, Move, FullMove, Square).


% A square is a possible source square of a move
% if it is occupied by the right piece (of the right color)
% and the square matches the move's source indicator
% TODO: and the resulting positon would be legal
possible_source_square(Move, Pos, Square) :-
    compatible_source_square(Move, Square),
    moved_piece(Move, Piece),
    position_turn(Pos, Color),
    board_occupant(Pos, Square, [Piece, Color]),
    legal(Move, Pos).
