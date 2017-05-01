
:- module(position, []).

%
%  This module handles updating a position, i.e after a move has been made
%

list_replace(Ix, Item, [X | List], [ X | Result]) :-
  nonvar(Ix), nonvar(Item),
  Ix > 0,
  succ(Ix0, Ix),
  list_replace(Ix0, Item, List, Result).

list_replace(0, Item, [_ | List], [Item | List]).


% Note: This function is inspired by piece_at predicate inside fen-module
% TODO refactor Y-coord transformation into separate predicate for reuse
board_replace([square, X, Y], NewPiece, [board, Rows], [board, NewRows]) :-
  movement:square(X, Y),

  fen:fen_y_coord(Y, Y0),
  
  nth0(Y0, Rows, Row),
  list_replace(X, NewPiece, Row, NewRow),
  list_replace(Y0, NewRow, Rows, NewRows).
