
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

