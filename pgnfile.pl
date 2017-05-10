:- module(pgnfile, []).

:- set_prolog_flag(double_quotes, codes).


meta(Key, Value) -->
  "[",
  meta_key(Key),
  " ",
  "\"",
  Value,
  "\"",
  "]".

meta_key(event).
meta_key(site).
meta_key(date).
meta_key(round).
meta_key(white).
meta_key(black).


meta_key(Key) -->
  {
    meta_key(Key),

    % TODO capitalize!
    atom_codes(Key, Codes),
  },
  Codes.

header([ [Key, Value] | KeyVals]) -->
  meta(Key, Value),
  header(KeyVals).

header([]) --> "\n".

pgnfile([pgnfile, KeyVals, Moves, Result]) -->
  header(KeyVals),
  pgnmoves(Moves),
  result(Result).


pgnmoves(Moves) --> []. % TODO

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
