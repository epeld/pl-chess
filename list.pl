first(L, X) :-
    nth0(0,L, X).

member(X, [X|_]).
member(X, [_|Rest]) :- member(X, Rest).

choose(0, _, []).
choose(N, Choices, Result) :-
    member(Choice, Choices),
    succ(Npred, N),
    choose(Npred, Choices, Rest),
    append([Choice], Rest, Result).


split([], _, []).
split([Part], _, Part).
split([Part, Part2 | Rest], Sep, L) :-
    append(A, B, L),
    append(Part, Sep, A),
    split([Part2 | Rest], Sep, B).

split([], _, [], 0).
split([Part], _, Part, 1).
split([Part, Part2 | Rest], Sep, L, N) :-
    length([Part, Part2 | Rest], N),
    append(A, B, L),
    append(Part, Sep, A),
    plus(Nminus, 1, N),
    split([Part2 | Rest], Sep, B, Nminus).

join([], _, []).
join([Part], _, Part).
join([Part, Part2 | Rest], Sep, L) :-
    append(Part, Sep, A),
    append(A, B, L),
    join([Part2 | Rest], Sep, B).


every(_, [], []).
every(Goal, [X | Xs], [Y | Ys]) :-
    call(Goal, X, Y),
    every(Goal, Xs, Ys).

every(_, []).
every(Goal, [X | Xs]) :-
    call(Goal, X),
    every(Goal, Xs).

:- begin_tests(list).

test(split2) :-
    split(["testing", "testing", "testing"], ",", S, _),
    length(S, 23).

test(split3) :-
    split(Parts, " ", "hej pa dig", 3),
    length(Parts, 3).

test(split) :-
    length(Parts, 3),
    split(Parts, " ", "hej pa dig").

test(join) :-
    join(["testing", "testing", "testing"], ",", S),
    length(S, 23).

:- end_tests(list).
