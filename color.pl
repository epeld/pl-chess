:- module(color, [color/1]).

color(white).
color(black).

opposite(white, black).
opposite(black, white).

last_pawn_rank(white, 7).
last_pawn_rank(black, 0).

first_pawn_rank(white, 2).
first_pawn_rank(black, 6).
