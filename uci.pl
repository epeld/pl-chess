:- module(uci, []).

:- set_prolog_flag(double_quotes, codes).

example_options("option name Use Debug Log type check default false
option name Use Search Log type check default false
option name Search Log Filename type string default SearchLog.txt
option name Book File type string default book.bin
option name Best Book Move type check default false
option name Contempt Factor type spin default 0 min -50 max 50
option name Mobility (Middle Game) type spin default 100 min 0 max 200
option name Mobility (Endgame) type spin default 100 min 0 max 200
option name Passed Pawns (Middle Game) type spin default 100 min 0 max 200
option name Passed Pawns (Endgame) type spin default 100 min 0 max 200
option name Space type spin default 100 min 0 max 200
option name Aggressiveness type spin default 100 min 0 max 200
option name Cowardice type spin default 100 min 0 max 200
option name Min Split Depth type spin default 4 min 4 max 12
option name Max Threads per Split Point type spin default 5 min 4 max 8
option name Threads type spin default 2 min 1 max 64
option name Use Sleeping Threads type check default true
option name Hash type spin default 32 min 1 max 8192
option name Clear Hash type button
option name Ponder type check default true
option name OwnBook type check default false
option name MultiPV type spin default 1 min 1 max 500
option name Skill Level type spin default 20 min 0 max 20
option name Emergency Move Horizon type spin default 40 min 0 max 50
option name Emergency Base Time type spin default 200 min 0 max 30000
option name Emergency Move Time type spin default 70 min 0 max 5000
option name Minimum Thinking Time type spin default 20 min 0 max 5000
option name Slow Mover type spin default 100 min 10 max 1000
option name UCI_Chess960 type check default false
option name UCI_AnalyseMode type check default false").

uci --> "uci".


option([Type, AName | Rest]) -->
  "option name ", string(Name), " type ", type(Type), 
  option_specific(Type, Rest),
  {
    atom_codes(AName, Name)
  }.


option_specific(spin, [Default, Min, Max]) -->
  " default ", fen:nat(Default), " min ", fen:nat(Min), " max ", fen:nat(Max).

option_specific(button, []) --> [].

option_specific(string, [Default]) -->
  " default ", string(Default).

option_specific(check, [Default]) -->
  " default ", boolean(Default).

type(spin) --> "spin".
type(button) --> "button".
type(string) --> "string".
type(check) --> "check".

string([]) --> [].
string([Char | Rest]) --> [Char], { Char \= 10 }, string(Rest).

boolean(true) --> "true".
boolean(false) --> "false".

newline --> "\r\n".
newline --> "\n".

on_off(on) --> "on".

on_off(off) --> "off".

isready --> "isready".

readyok --> "readyok".

setoption(Name, Value) -->
  "setoption ",
  Name,
  " value ",
  Value.

ucinewgame --> "ucinewgame".


position(startpos, Moves) -->
  "position startpos moves ",
  move_list(Moves).


go --> "go".


main(_) :-
  format("Hello World\n").
