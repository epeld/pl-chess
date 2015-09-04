:- module(listutils, [sublist/2, filled/2]).

:- use_module(library(clpfd)).

% sublist(L, L2) means "L2 is a sublist of L"
sublist([A | As], [A | Bs]) :- sublist(As, Bs). 
sublist([_ | As], [B | Bs]) :- sublist(As, [B | Bs]). 
sublist(_, []). 


filled(_, []).
filled(X, [X | L]) :- filled(X, L).
filled(X, L, N) :- length(L, N), filled(X, L).


replacement_at(_, [], [], _).
replacement_at(0, [_ | L], [El | L], El).
replacement_at(Ix, [A | L], [A | L2], El) :-
    Ix #> 0, Ix2 #= Ix - 1,
    replace(Ix2, L, L2, El).
