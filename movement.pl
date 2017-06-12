:- module(movement, []).

:- set_prolog_flag(double_quotes, codes).

knights_jump(square(X1, Y1), square(X2, Y2)) :-
  square(X1, Y1), square(X2, Y2),

  plus(XDif, X1, X2),
  plus(YDif, Y1, Y2),

  abs(XDif, XDif2),
  abs(YDif, YDif2),

  XDif2 > 0, YDif2 > 0,

  plus(XDif2, YDif2, 3).


diagonal(StartSquare, EndSquare, Diagonal, Direction) :-
  Diagonal = [ StartSquare | Rest ],
  diagonal(Diagonal, Direction),
  append(_, [EndSquare], Rest).


diagonal([ Square | Diagonal], Direction) :-
  square(Square),
  member(Direction, [ up_right, up_left, down_right, down_left ]),
  sequence(Square, Direction, Diagonal).


line(StartSquare, EndSquare, Line, Direction) :-
  Line = [StartSquare | Rest],
  line(Line, Direction),
  append(_, [EndSquare], Rest).


line([ Square | Line], Direction) :-
  square(Square),
  member(Direction, [up, down, left, right]),
  sequence(Square, Direction, Line).


sequence(square(X, Y), Direction, [square(X2, Y2) | Squares]) :-
  offset(X, Y, X2, Y2, Direction),
  sequence(square(X2, Y2), Direction, Squares).


sequence(_, _, []).

square(square(X, Y)) :-
  between(0, 7, X),
  between(0, 7, Y).


square(X, Y) :-
  between(0, 7, X),
  between(0, 7, Y).


%
% Diagonal
%

offset(X, Y, X2, Y2, up_right) :-
  succ(X, X2),
  succ(Y, Y2),
  square(X, Y), square(X2, Y2).

offset(X, Y, X2, Y2, up_left) :-
  succ(X2, X),
  succ(Y, Y2),
  square(X, Y), square(X2, Y2).


offset(X, Y, X2, Y2, down_right) :-
  offset(X2, Y2, X, Y, up_left).


offset(X, Y, X2, Y2, down_left) :-
  offset(X2, Y2, X, Y, up_right).

%
% Straight
%
offset(X, Y, X2, Y, right) :-
  succ(X, X2),
  square(X, Y), square(X2, Y).


offset(X, Y, X2, Y2, left) :-
  offset(X2, Y2, X, Y, right).


offset(X, Y, X, Y2, up) :-
  succ(Y, Y2),
  square(X, Y), square(X, Y2).


offset(X, Y, X2, Y2, down) :-
  offset(X2, Y2, X, Y, up).


offset(square(X, Y), square(X2, Y2), Dir) :-
  offset(X, Y, X2, Y2, Dir).


%
%  Pawn Logic
%
pawn_move_square(white, square(X, Y), square(X, Y2)) :-
  succ(Y, Y2).

% Second rank pawns can walk two steps
pawn_move_square(white, square(X, 1), square(X, 3)).

% Seventh rank pawns can walk two steps
pawn_move_square(black, square(X, 6), square(X, 4)).

pawn_move_square(black, square(X, Y), square(X, Y2)) :-
  succ(Y2, Y).


passant_square(white, square(X, 1), square(X, 3), square(X, 2)).
passant_square(black, square(X, 6), square(X, 4), square(X, 5)).

passant_square(black, square(_, 6), square(_, 5), nothing).
passant_square(white, square(_, 1), square(_, 2), nothing).

passant_square(black, square(_, Y), _, nothing) :-
  member(Y, [1,2,3,4,5]).

passant_square(white, square(_, Y), _, nothing) :-
  member(Y, [2,3,4,5,6]).




pawn_square(capture, Color, Sq, Sq2) :- pawn_capture_square(Color, Sq, Sq2).
pawn_square(move, Color, Sq, Sq2) :- pawn_move_square(Color, Sq, Sq2).

pawn_capture_square(Color, Sq, Sq2) :-
  pawn_direction(capture, Color, Direction),
  offset(Sq, Sq2, Direction).


pawn_direction(move, white, up).
pawn_direction(move, black, down).

pawn_direction(capture, white, up_right).
pawn_direction(capture, white, up_left).

pawn_direction(capture, black, down_right).
pawn_direction(capture, black, down_left).




officer(bishop).
officer(knight).
officer(queen).
officer(rook).
officer(king).
