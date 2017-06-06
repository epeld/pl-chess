:- module(uci, []).

%
% This module is for parsing and encoding the UCI protocol
%

:- set_prolog_flag(double_quotes, codes).


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

