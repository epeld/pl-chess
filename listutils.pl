
% sublist(L, L2) means "L2 is a sublist of L"
sublist([A | As], [A | Bs]) :- sublist(As, Bs). 
sublist([_ | As], [B | Bs]) :- sublist(As, [B | Bs]). 
sublist(_, []). 


filled(_, []).
filled(X, [X | L]) :- filled(X, L).
filled(X, L, N) :- length(L, N), filled(X, L).


split_chars(Full, Sep, Parts) :-
    ground(Full),
    atom_chars(FullAtom, Full),
    atomic_list_concat(AtomParts, Sep, FullAtom),
    maplist(atom_chars, AtomParts, Parts).


split_chars(Full, Sep, Parts) :-
    ground(Parts),
    maplist(atom_chars, AtomParts, Parts),
    atomic_list_concat(AtomParts, Sep, FullAtom),
    atom_chars(FullAtom, Full).
