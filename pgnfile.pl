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

metas([[Key, Value] | Rest]) -->
  meta(Key, Value),
  metas0(Rest).


metas0([[Key, Value] | Rest]) -->
  newline,
  meta(Key, Value),
  metas0(Rest).

metas0([]) --> newline.

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

    % TODO capitalize!
    atom_codes(Key, Codes),
    capitalized_string(Codes, CapCodes)
  },
  CapCodes.

header([ [Key, Value] | KeyVals]) -->
  meta(Key, Value),
  header(KeyVals).

header([]) --> "\n".

pgnfile([pgnfile, KeyVals, Moves, Result]) -->
  header(KeyVals),
  pgnmoves(Moves),
  result(Result).


pgnmoves(Moves) --> []. 

result(white) --> "1-0".
result(black) --> "0-1".
result(draw) --> "1/2-1/2".
result(unknown) --> [].


movenr(Number, white, Before, After) :-
  nonvar(Number),
  !,
  number_codes(Number, Codes),
  fen:digits(Codes, Before, After).


movenr(Number, white, Before, After) :-
  var(Number),
  fen:digits(Codes, Before, After),
  number_codes(Number, Codes).
  

movenr(Number, black, Before, After) :-
  phrase( (movenr(Number, white), ".."), Before, After).
