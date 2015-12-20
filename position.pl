
:- module(position, [
        parts/2, 
        board/2, 
        board_occupants/2, 
        board_occupant/3
    ]).

:- use_module(listutils).
:- use_module(square).
:- use_module(maybe).
:- use_module(color).
:- use_module(piece).
:- use_module(library(clpfd)).

replaced_board(Position, NewBoard, NewPosition) :-
    parts(Position, Parts),
    listutils:replacement_at(0, Parts, NewParts, NewBoard),
    parts(NewPosition, NewParts).


board_move(Board, Source, Target, NewBoard) :-
    board_occupant(Board, Square, Piece),
    board_put(Board, Source, nothing, B2),
    board_put(B2, Target, Piece, NewBoard).


board_put(Board, Square, Piece, NewBoard) :-
    square_index(Square, Ix),
    replacement_at(Ix, Board, NewBoard, Piece).

put(Position, Square, Piece, Position2) :-
    parts(Position, [Board | Rest]),

    board_put(Board, Square, Piece, Board2),

    parts(Position2, [Board2 | Rest]).


move(Position, Source, Target, NewPosition) :-
    board(Position, Board),
    board_move(Board, Source, Target, NewBoard),

    parts(Position, Parts),
    listutils:replacement_at(0, Parts, NewParts, NewBoard),
    parts(NewPosition, NewParts).

    

% A position consists of the 6 parts that can be parsed out of a FEN string
% See the fen-module
parts([position | Parts], Parts) :- 
    length(Parts, 6),

    Parts = [Board, Turn, Rights, Passant, FullMove, HalfMove],

    board(Board), turn(Turn), castling_rights(Rights),
    passant_square(Passant), move_number(FullMove), move_number(HalfMove).

board(Board) :- 
    length(Board, 64),
    apply:maplist(maybe(piece), Board).

turn(Turn) :- color:color(Turn).

castling_rights(Rights) :- sublist(['K', 'Q', 'k', 'q'], Rights).

passant_square(X) :- maybe(square:square, X).

move_number(X) :- X #> 0.


board(Position, Board) :- parts(Position, Parts), nth0(0, Parts, Board).


turn(Position, white) :- parts(Position, Parts), nth0(1, Parts, white).
turn(Position, black) :- parts(Position, Parts), nth0(1, Parts, black).


occupants(Position, Occupants) :- % shortcut: treat position as board 
    board(Position, Board), 
    board_occupants(Board, Occupants).

occupant(Position, Square, Piece) :-
    board(Position, Board), 
    board_occupant(Board, Square, Piece).

board_occupants([board | Occupants], Occupants) :- length(Occupants, 64).

board_occupant(Board, Square, Piece) :- 
    board_occupants(Board, Occupants),
    square:square_index(Square, Index),
    nth0(Index, Occupants, Piece).
