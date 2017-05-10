:- module(zipper, []).

%
% This module defines a data structure for moving back and forth along a line of
% - say - positions in a chess game!
%

zsingleton([Value, [], []], Value).

zappend(Items,
        [Value, Backward, Forward],
        [Value, Backward, Forward2]) :-
  
  append(Forward, Items, Forward2).


zforward([Value, Backward, [Item | Forward]], [Item, [ Value | Backward ], Forward]).


zipper([Value, Backward, Forward], Value, Backward, Forward).

zvalue([Value, _, _], Value).

% zput/3 is like zappend/3 except it expects the zipper to be forwarded to the end
zput([V, History, []], [V, History, Values], Values).


zset([_, History, Future], [NewValue, History, Future], NewValue).


zfastforward([Value, History, Forward], [Last, History2, []]) :-
  ground(History), ground(Forward),
  reverse(Forward, [ Last | ForwardR ]),
  append(ForwardR, [ Value | History ], History2).

zfastforward([Value, History, []], [Value, History, []]).


zrewind([Value, History, Forward], [Initial, [], Forward2]) :-
  ground(History), ground(Forward),
  reverse(History, [Initial | HistoryR]),
  append(HistoryR, [ Value | Forward ], Forward2).

zrewind([Value, [], Forward], [Value, [], Forward]).


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
