:- module(piece_type, [piece/1, piece_type/1]).

:- use_module(color, [color/1]).

piece([Color, PieceType]) :-
    color(Color), 
    piece_type(PieceType).

color([Color, T], Color) :-
    piece([Color, T]).

type([Color, T], T) :-
    piece([Color, T]).


piece_type(pawn).
piece_type(bishop).
piece_type(knight).
piece_type(rook).
piece_type(queen).
piece_type(king).

