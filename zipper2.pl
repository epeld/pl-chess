:- module(zipper2, []).

%
% This module defines a data structure for moving back and forth along a line of
% - say - positions in a chess game!
%

zsingleton(zipper(Value, [], []), Value).

zappend(Items,
        zipper(Value, Backward, Forward),
        zipper(Value, Backward, Forward2)) :-
  
  append(Forward, Items, Forward2).


zforward(zipper(Value, Backward, [Item | Forward]), zipper(Item, [ Value | Backward ], Forward)).


zipper(zipper(Value, Backward, Forward), Value, Backward, Forward).

zvalue(zipper(Value, _, _), Value).

% zput/3 is like zappend/3 except it expects the zipper to be forwarded to the end
zput(zipper(V, History, []), zipper(V, History, Values), Values).


zset(zipper(_, History, Future), zipper(NewValue, History, Future), NewValue).


zfastforward(zipper(Value, History, Forward), zipper(Last, History2, [])) :-
  ground(History), ground(Forward),
  reverse(Forward, [ Last | ForwardR ]),
  append(ForwardR, [ Value | History ], History2).

zfastforward(zipper(Value, History, []), zipper(Value, History, [])).


zrewind(zipper(Value, History, Forward), zipper(Initial, [], Forward2)) :-
  ground(History), ground(Forward),
  reverse(History, [Initial | HistoryR]),
  append(HistoryR, [ Value | Forward ], Forward2).

zrewind(zipper(Value, [], Forward), zipper(Value, [], Forward)).


%
% Unify Member with some configuration of the current Zipper
% in terms of forward/backward movements
zconfiguration(Zipper, Configuration) :-
  zrewind(Zipper, Z1),
  zconfiguration_(Z1, Configuration).

zconfiguration_(Z1, Z1).

zconfiguration_(Z1, Configuration) :-
  ground(Z1),
  zforward(Z1, Z2),
  zconfiguration_(Z2, Configuration).
