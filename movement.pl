:- module(movement, []).


knights_jump([square, X1, Y1], [square, X2, Y2]) :-
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


sequence(Square, Direction, [Square2 | Squares]) :-
  offset(Square, Square2, Direction),
  sequence(Square2, Direction, Squares).


sequence(_, _, []).

square([square, X, Y]) :-
  between(0, 7, X),
  between(0, 7, Y).


square(X, Y) :-
  between(0, 7, X),
  between(0, 7, Y).


%
% Diagonal
%
offset([square, X, Y], [square, X2, Y2], up_right) :-
  square(X, Y), square(X2, Y2),
  succ(X, X2),
  succ(Y, Y2).


offset([square, X, Y], [square, X2, Y2], up_left) :-
  square(X, Y), square(X2, Y2),
  succ(X2, X),
  succ(Y, Y2).


offset([square, X, Y], [square, X2, Y2], down_right) :-
  square(X, Y), square(X2, Y2),
  succ(X, X2),
  succ(Y2, Y).


offset([square, X, Y], [square, X2, Y2], down_left) :-
  square(X, Y), square(X2, Y2),
  succ(X2, X),
  succ(Y2, Y).

%
% Straight
%
offset([square, X, Y], [square, X2, Y], right) :-
  square(X, Y), square(X2, Y),
  succ(X, X2).


offset([square, X, Y], [square, X2, Y], left) :-
  square(X, Y), square(X2, Y),
  succ(X2, X).


offset([square, X, Y], [square, X, Y2], up) :-
  square(X, Y), square(X, Y2),
  succ(Y, Y2).


offset([square, X, Y], [square, X, Y2], down) :-
  square(X, Y), square(X, Y2),
  succ(Y2, Y).




%
%  Pawn Logic
%
pawn_move_square(white, [square, X, Y], [square, X, Y2]) :-
  succ(Y, Y2).

pawn_move_square(white, [square, X, Y], [square, X, Y2]) :-
  % Second rank pawns can walk two steps
  fen:rank(Y, "2", []),
  succ(Y, Y1), succ(Y1, Y2).

pawn_move_square(black, [square, X, Y], [square, X, Y2]) :-
  % Seventh rank pawns can walk two steps
  fen:rank(Y, "7", []),
  succ(Y2, Y1), succ(Y1, Y).

pawn_move_square(black, [square, X, Y], [square, X, Y2]) :-
  succ(Y2, Y).



passant_square(Turn, Source, Destination, Passant) :-
  pawn_move_square(Turn, Source, Destination),
  
  line(Source, Destination, Line, _),
  ( append([[Source], [Passant], [Destination]], Line)
  
  ; Passant = nothing ).


pawn_square(capture, Color, Sq, Sq2) :- pawn_capture_square(Color, Sq, Sq2).
pawn_square(move, Color, Sq, Sq2) :- pawn_move_square(Color, Sq, Sq2).

pawn_capture_square(Color, Sq, Sq2) :-
  pawn_direction(capture, Color, Direction),
  diagonal(Sq, Sq2, [Sq, Sq2], Direction).


pawn_direction(move, white, up).
pawn_direction(move, black, down).

pawn_direction(capture, white, up_right).
pawn_direction(capture, white, up_left).

pawn_direction(capture, black, down_right).
pawn_direction(capture, black, down_left).




officer(rook).
officer(bishop).
officer(queen).
officer(king).
officer(knight).
