
:- module(position, [
        parts/2, 
        board/2, 
        board_occupants/2, 
        board_occupant/3
    ]).

:- use_module(square).

% A position consists of the 6 parts that can be parsed out of a FEN string
% See the fen-module
parts([position | Parts], Parts) :- length(Parts, 6).


board(Position, Board) :- parts(Position, Parts), nth0(0, Parts, Board).


turn(Position, Turn) :- parts(Position, Parts), nth0(1, Parts, Turn).


board_occupants([board | Occupants], Occupants) :- length(Occupants, 64).
board_occupants(Position, Occupants) :- % shortcut: treat position as board 
    board(Position, Board), 
    board_occupants(Board, Occupants).


board_occupant(Board, Square, Piece) :- 
    board_occupants(Board, Occupants),
    square:square_index(Square, Index),
    nth0(Index, Occupants, Piece).
