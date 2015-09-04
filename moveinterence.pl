
:- module(moveinference, [
        uniquely_determined/2
    ]).

:- use_module(pgnmove).
:- use_module(movement).
:- use_module(square).
:- use_module(listutils).


% Determine the least sufficient source indicator that
% uniquely determines FullMove in Position,
% Example:
% given FullMove = Ne2xd4, Indicator might be 'e',
% meaning Nexd4 would be enough to determine the intended move
sufficient_indicator(Indicator, Position, FullMove) :-
    bagof(Indicator, blabla, _). % TODO


% FullMove is like move, except that its source is
% the uniquely determined source square of Move
uniquely_determined_move(Move, Position, FullMove) :-
    uniquely_determined(source(Move, Position, Square),
    source_square(Move, Position, Square).


% A Move is uniquely determined in a given position
% whenever it has only one legal_source_square.
uniquely_determined_source(Move, Position, UniqueSource) :-
    bagof(X, legal_source_square(Move, Position, X), [UniqueSource]).


% A legal source square is a candidate source square
% that, when when used with Move, results in a legal position
legal_source_square(Move, Pos, Square) :-
    candidate_square(Move, Pos, Square),
    pgnmove:source_square(Move, Pos, FullMove),
    legal(FullMove, Pos).


% TODO create legality-module
% TODO determine if a move is legal in a given position
legal(_, _).


% A candidate source square of a Move is a source square
% that is occupied by a piece that is capable of
% performing the Move in question
candidate_source_square(Move, Pos, Square) :-
    pgnmove:source_square(Move, Square),
    pgnmove:moved_piece(Move, Piece),
    position_turn(Pos, Color),
    board_occupant(Pos, Square, [Piece, Color]).
