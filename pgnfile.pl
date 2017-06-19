:- module(pgnfile, []).

:- set_prolog_flag(double_quotes, codes).

:- use_module(fen).

% TODO introduce annotated_pgnmove here

pgn_move_string(Position, Moves, String) :-
  fen:turn(Position, Turn),
  fen:full_move(Position, Nr),
  move_pairs(Nr, Turn, Moves, String, []).


move_pairs(Nr0, black, [Black]) -->
  move_pair(Nr0, none, Black).


move_pairs(Nr0, white, [White]) -->
  move_pair(Nr0, White, none).

move_pairs(Nr0, white, [White, Black | Moves]) -->
  move_pair(Nr0, White, Black),
  { succ(Nr0, Nr) },
  ( { Moves \= [] }, " ", move_pairs(Nr, white, Moves)
  ; { Moves = [] }).


move_pairs(Nr0, black, [Black | Moves]) -->
  move_pair(Nr0, none, Black),
  { succ(Nr0, Nr) },
  ( { Moves \= [] }, " ", move_pairs(Nr, white, Moves)
  ; { Moves = [] }).


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
  maybe_space.


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

  ( maybe_space, fancy_move(WhiteMove), space
  ; {WhiteMove = none}, ". " ),
  
  ( fancy_move(BlackMove) ;
    { BlackMove = none } ),
  
  maybe_space.

fancy_move(Move) -->
  pgn:move(Move),
  ([] ; check_indicator(_)).

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
