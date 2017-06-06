:- module(uci, []).

%
% This module is for parsing and encoding the UCI protocol
%

:- set_prolog_flag(double_quotes, codes).

:- use_module(fen).


uci --> "uci".


option(option(AName, Specific)) -->
  "option name ", string(Name), " type ", type(Type), 
  option_specific(Type, Specific),
  {
    atom_codes(AName, Name)
  }.


option_specific(spin, spin(Default, Min, Max)) -->
  " default ", fen:nat(Default), " min ", fen:nat(Min), " max ", fen:nat(Max).

option_specific(button, button) --> [].

option_specific(string, string(Default)) -->
  " default ", string(Default).

option_specific(check, check(Default)) -->
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

setoption(Name, Value) -->
  "setoption ",
  Name,
  " value ",
  Value.

ucinewgame --> "ucinewgame".


position(startpos, Moves) -->
  "position startpos moves ",
  move_list(Moves).


bestmove(bestmove(Moves, Ponder)) -->
  "bestmove ",
  uci_moves(Moves),
  
  ( " " ; [] ),
  
  ( ponder(Ponder)
  ; [], {Ponder = none}).

ponder(none) --> "ponder (none)".
ponder(Move) --> "ponder ", uci_move(Move).

uci_moves([Move, Move2 | Moves]) -->
  uci_move(Move),
  " ",
  uci_moves([Move2 | Moves]).

uci_moves([Move]) --> uci_move(Move).
uci_moves([]) --> [].

uci_move(From-To) -->
  fen:square(From),
  fen:square(To).
