:- module(pgnfile, []).

:- set_prolog_flag(double_quotes, codes).

example_metas("[Event \"Earl tourn\"]
[Site \"?\"]
[Date \"1906.??.??\"]
[Round \"?\"]
[White \"Savrov\"]
[Black \"Alekhine, Alexander\"]
[Result \"0-1\"]
[WhiteElo \"\"]
[BlackElo \"\"]
[ECO \"C30\"]").

example_moves("1.e4 e5 2.f4 Bc5 3.Nf3 d6 4.c3 Bg4 5.Be2 Bxf3 6.Bxf3 Nc6 7.b4 Bb6 8.b5 Nce7 9.d4 exd4 10.cxd4 Nf6 11.O-O O-O 12.Bb2 d5 13.e5 Nd7 14.Nc3 c6 15.Qd3 Ng6 16.g3 f5 17.Kh1 Qe7 18.Nxd5 cxd5 19.Bxd5+ Kh8 20.Ba3 Qd8 21.Bxb7 Rb8 22.Bxf8 Ndxf8 23.Bc6 Qxd4 24.Qxf5 Ne7 25.Qc2 Rc8 26.Rad1 Qb4 27.Rb1 Qa5 28.Rf3 Nxc6 29.Rc3 Rd8 30.bxc6 Qd5+ 31.Qg2 Qd1+ 32.Qf1 Qd5+ 33.Qf3 Qxa2 34.Rd1 Rxd1+ 35.Qxd1 Ne6 36.Qf3 Qb1+ 37.Kg2 g6 38.Qd3 Qg1+ 39.Kh3 h5 40.Qc4 Qd1 41.Rc1 Qg4+ 42.Kg2 h4 43.Qf1 g5 44.Qd1 Nxf4+ 45.Kh1 0-1").

metas([[Key, Value] | Rest]) -->
  meta(Key, Value),
  metas0(Rest).


metas0([[Key, Value] | Rest]) -->
  newline,
  meta(Key, Value),
  metas0(Rest).

metas0([]) --> newline.
metas0([]) --> newline, newline.

newline --> "\n".
newline --> "\r\n".

meta(Key, Value) -->
  "[",
  meta_key(Key),
  " ",
  "\"",
  value_string(Value),
  "\"",
  "]".


value_string([Char | Rest]) -->
  value_char(Char),
  value_string(Rest).


value_string([]) --> [].


value_char(Char) -->
  {
    value_char(Char)
  },
  [Char].

value_char(Char) :- alphabet(A), member(Char, A).
value_char(Char) :- numeric(Char).
value_char(Char) :- member(Char, "? _-.,\\/}{}:;'~").

numeric(Char) :- member(Char, "0123456789").

lower_alphabet("abcdefghijklmnopqrstuvwxyz").
upper_alphabet("ABCDEFGHIJKLMNOPQRSTUVWXYZ").

alphabet(A) :- lower_alphabet(A).
alphabet(A) :- upper_alphabet(A).

capitalized(Char, Capitalized) :-
  alphabet(A),
  upper_alphabet(UA),
  nth0(Ix, A, Char),
  nth0(Ix, UA, Capitalized).

capitalized_string([Char | Rest], [Cap | Rest]) :-
  capitalized(Char, Cap).

meta_key(event).
meta_key(site).
meta_key(date).
meta_key(round).
meta_key(white).
meta_key(black).
meta_key(result).
meta_key(whiteElo).
meta_key(blackElo).
meta_key(eCO).


meta_key(Key) -->
  {
    meta_key(Key),

    atom_codes(Key, Codes),
    capitalized_string(Codes, CapCodes)
  },
  CapCodes.

pgnfile([pgnfile, KeyVals, Moves, Result]) -->
  metas(KeyVals),
  pgnmoves(Moves),
  maybe_space,
  result(Result),
  maybe_space,
  !.


pgnmoves([[White, Black] | Moves], B, A) :-
  phrase(move_pair(_, White, Black), B, A0),
  phrase(pgnmoves(Moves), A0, A).

pgnmoves([]) --> [].

result(white) --> "1-0".
result(black) --> "0-1".
result(draw) --> "1/2-1/2".
result(unknown) --> [].

move_pair(Number, WhiteMove, BlackMove) -->
  maybe_space,
  movenr(Number, white),
  maybe_space,
  fancy_move(WhiteMove),
  
  ( space, fancy_move(BlackMove) ;
    { BlackMove = none } ),
  
  maybe_space.

fancy_move(Move) -->
  pgn:move(Move),
  (check_indicator(_) ; []).

check_indicator(check) --> "+".
check_indicator(mate) --> "#".

movenr0(Number, white, Before, After) :-
  nonvar(Number),
  !,
  number_codes(Number, Codes),
  fen:digits(Codes, Before, After).


movenr0(Number, white, Before, After) :-
  var(Number),
  fen:digits(Codes, Before, After),
  number_codes(Number, Codes).
  

movenr(Number, white, Before, After) :-
  phrase( (movenr0(Number, white), ("."; [])), Before, After).

movenr(Number, black, Before, After) :-
  phrase( (movenr0(Number, white), ".."), Before, After).


maybe_space --> space.
maybe_space --> newline.
maybe_space --> [].

space --> (" " ; newline), space0.

space0(X, X) :- var(X).

space0(Before, After) :-
  nonvar(Before),
  phrase(many_spaces, Before, After).


many_spaces --> ( " " ; newline ), many_spaces.
many_spaces --> [].
