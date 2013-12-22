
number_term("0", 0).
number_term("1", 1).
number_term("2", 2).
number_term("3", 3).
number_term("4", 4).
number_term("5", 5).
number_term("6", 6).
number_term("7", 7).
number_term("8", 8).
number_term("9", 9).

number_term(X) :- number_term(X, _).


is_lower(X) :-
    between(97,122, X).

is_upper(X) :-
    between(65,90, X).


term_upper([], []).

term_upper([Term | Rest], [Term | UpperRest]) :-
    is_upper(Term),
    term_upper(Rest, UpperRest).

term_upper([Term | Rest], [UpperTerm | UpperRest]) :-
    is_lower(Term),
    first("a", A),
    first("A", Big_A),
    plus(A, Diff, Big_A),
    plus(Term, Diff, UpperTerm),
    term_upper(Rest, UpperRest).

term_lower([], []).

term_lower([Term | Rest], [Term | UpperRest]) :-
    is_lower(Term),
    term_upper(Rest, UpperRest).

term_lower([Term | Rest], [LowerTerm | LowerRest]) :-
    is_upper(Term),
    first("a", A),
    first("A", Big_A),
    plus(Big_A, Diff, A),
    plus(Term, Diff, LowerTerm),
    term_lower(Rest, LowerRest).

term_member(X, T) :-
    nth0(0,X, Xchar),
    member(Xchar, T).
