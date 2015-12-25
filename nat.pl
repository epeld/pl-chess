
nat(zero).
nat(s(N)) :- nat(N).


greater(s(_), zero).
greater(s(N), s(M)) :- greater(N, M).

nonzero(s(_)).

greater_than_or_equal(X, Y) :- greater(X, Y) ; X = Y.


less(X, Y) :- greater(Y, X).

less_than_or_equal(X, Y) :- less(X, Y) ; X = Y.


add(zero, Y, Y).
add(s(N), Y, Sum) :- greater(Sum, Y), add(N, s(Y), Sum).


multiply(zero, _, zero).
multiply(s(X), Y, Product) :- greater_than_or_equal(Product, Y), multiply(X, Y, Part), add(Y, Part, Product).


drop(zero, L, L).
drop(s(N), [_ | L], R) :- drop(N, L, R).


head(H, [H | _]).

len([], zero).
len([_ | Rest], s(N)) :- len(Rest, N).


nat_to_digit(N, Digit) :- drop(N, [0,1,2,3,4,5,6,7,8,9], [Digit | _]).

decimal_nat(N) :-
    nat_to_digit(One, 1),
    nat_to_digit(Nine, 9),
    less_than_or_equal(One, N),
    less_than_or_equal(N, Nine).

nats_to_digits([N], [Digit]) :- nat_to_digit(N, Digit).
nats_to_digits([N | Ns], [Digit | Digits]) :- 
    same_length(Ns, Digits),

    nat_to_digit(N, Digit), 
    decimal_nat(N),
    nats_to_digits(Ns, Digits).


digit(X) :- memberchk(X, [0,1,2,3,4,5,6,7,8,9]).

arabic([]).
arabic([Digit | Digits]) :- digit(Digit), arabic(Digits).

arabic(Digits, Number) :- 
    decimals(Ns, Number),
    nats_to_digits(Ns, Digits).


sum([], zero).

sum([zero | Ns], Sum) :- sum(Ns, Sum).

sum([s(X) | Ns], Sum) :- 
    less(X, Sum),

    add(s(X), PartialSum, Sum),
    sum(Ns, PartialSum).


decimals(Ns, Result) :-
    head(s(_), Ns),

    reverse(Ns, RNs),
    reversed_decimals(RNs, Result).

reversed_decimals([s(N)], s(N)) :- ten(Ten), less(s(N), Ten).

reversed_decimals([N | Ns], Result) :- 
    non_empty(Ns),

    % Limit the search space
    ten(Ten),
    less(N, Ten),
    %len([N | Ns], Len),
    %power(Ten, Len, UpperBound),
    %greater(UpperBound, Result),

    add(N, PartialResult10, Result),
    multiply(Ten, PartialResult, PartialResult10),
    reversed_decimals(Ns, PartialResult).


power(_, zero, s(zero)).
power(Exponent, s(zero), Exponent).
power(Exponent, s(s(Log)), Value) :-
    power(Exponent, s(Log), Rest),
    multiply(Exponent, Rest, Value).


perform(X=Y) :- 
    calculate(X, Result1), 
    calculate(Y, Result1).

calculate(Arabic, Result) :- arabic(Result, Arabic).

calculate(X + Y, Res) :- 
    calculate(X, XRes), 
    calculate(Y, YRes), 
    add(XRes, YRes, Res).

calculate(X * Y, Res) :- 
    calculate(X, XRes), 
    calculate(Y, YRes), 
    multiply(XRes, YRes, Res).


one(s(zero)).
two(s(s(zero))).
three(s(Two)) :- two(Two).
four(s(Three)) :- three(Three).
five(s(Four)) :- four(Four).
six(s(Five)) :- five(Five).
seven(s(Six)) :- six(Six).
eight(s(Seven)) :- seven(Seven).
nine(s(Eight)) :- eight(Eight).
ten(s(Nine)) :- nine(Nine).

non_empty([_ | _]).
