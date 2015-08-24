
% sublist(L, L2) means "L2 is a sublist of L"
sublist([A | As], [A | Bs]) :- sublist(As, Bs). 
sublist([_ | As], [B | Bs]) :- sublist(As, [B | Bs]). 
sublist(_, []). 


filled(_, []).
filled(X, [X | L]) :- filled(X, L).
filled(X, L, N) :- length(L, N), filled(X, L).
