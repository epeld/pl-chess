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


number(Nr) -->
  fen:nat(Nr).

number(Nr) --> "-", fen:nat(Nr0), { Nr is -Nr0 }.

option_specific(spin, spin(Default, Min, Max)) -->
  " default ", number(Default), " min ", number(Min), " max ", number(Max).

% TODO combo option

option_specific(button, button) --> [].

option_specific(string, string(Default)) -->
  " default ", string(Default).

option_specific(check, check(Default)) -->
  " default ", boolean(Default).

type(spin) --> "spin".
type(combo) --> "combo".
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


%
% Bestmove
%
bestmove(bestmove(Moves, Ponder)) -->
  "bestmove ",
  uci_moves(Moves),
  
  ( " " ; [] ),
  
  ( ponder(Ponder)
  ; [], {Ponder = none}).

ponder(none) --> "ponder (none)".
ponder(Move) --> "ponder ", uci_move(Move).

%
% Move Parsing
%
uci_moves([Move, Move2 | Moves]) -->
  uci_move(Move),
  " ",
  uci_moves([Move2 | Moves]).

uci_moves([Move]) --> uci_move(Move).
uci_moves([]) --> [].

uci_move(From-To) -->
  fen:square(From),
  fen:square(To).

uci_move(From-To = Promotion) -->
  uci_move(From - To),
  promotion(Promotion).

promotion(rook) --> "r".
promotion(bishop) --> "b".
promotion(knight) --> "n".
promotion(queen) --> "q".

%
% Id Parsing
%
id_string(name(String)) -->
  "id name ", string(String).

id_string(author(String)) -->
  "id author ", string(String).


%
% Info String Parsing
%
info_line([Info | Infos]) -->
  "info ",
  infos([Info | Infos]).

infos([Info | Infos]) --> info(Info), infos1(Infos).

infos1([]) --> [].
infos1([Info | Infos]) --> " ", info(Info), infos1(Infos).

info(score(Score)) -->
  "score ", score(Score).

info(pv(Moves)) -->
  "pv ", uci_moves(Moves).

info(refutation(Moves)) -->
  "refutation ", uci_moves(Moves).

info(string(String), Line, []) :-
  append("string ", String, Line).

info(currline(N, Moves)) -->
  "currline ",
  fen:nat(N),
  " ",
  uci_moves(Moves).

info(currline(1, Moves)) -->
  "currline ",
  uci_moves(Moves).

info(currmove(Move)) -->
  "currmove ", uci_move(Move).

info(Info) -->
  nat_info(Info).

nat_info_atom(Atom, Name) :-
  nat_info_atom(Atom),
  atom_codes(Atom, Name).


base_score(mate(InN)) -->
  "mate ", fen:nat(InN).

base_score(cp(CentiPawns)) -->
  "cp ", number(CentiPawns).

score(Score) -->
  base_score(Score).

score(lowerbound(Score)) -->
  base_score(Score), " lowerbound".

score(upperbound(Score)) -->
  base_score(Score), " upperbound".


nat_info(Functor) -->
  nat_info_functor(Functor, N),
  " ",
  fen:nat(N).

% Parse a nat_info
nat_info_functor(Functor, N, Before, After) :-
  nat_info_functor(Functor, Name, N),
  append(Name, After, Before).

nat_info_functor(Functor, Name, N) :-
  nat_info_atom(Atom, Name),
  functor(Functor, Atom, 1),
  arg(1, Functor, N).


% All the infos that have a natural number as value
nat_info_atom(seldepth).
nat_info_atom(depth).
nat_info_atom(currmovenumber).
nat_info_atom(multipv).
nat_info_atom(nodes).
nat_info_atom(time).
nat_info_atom(hashfull).
nat_info_atom(cpuload).
nat_info_atom(nps).
nat_info_atom(tbhits).
nat_info_atom(sbhits).


% TODO write a program to parse the uci spec to retrieve the documentation for each "info" the engine can send (see Notes.md)


uci_pgn_moves(P, [Uci1 | UciMoves], [Pgn1 | PgnMoves]) :-
  uci_pgn_move(P, Uci1, Pgn1),
  pgn:make_move(Pgn1, P, P2),
  uci_pgn_move(P2, UciMoves, PgnMoves).

uci_pgn_moves(_P, [], []).

uci_pgn_move(P, Source - Dest, Pgn) :-
  % TODO write a 'full_move-like' predicate that finds the shorted unambiguous pgn mv
  % TODO promo
  ( pgn:full_move(P, [move, pawn, Source, _, Dest, nothing], Pgn)
  ; pgn:full_move(P, [move, _, Source, _, Dest], Pgn)).


uci_pgn_move(P, Source - Dest = Promotion, Pgn) :-
  % TODO write a 'full_move-like' predicate that finds the shorted unambiguous pgn mv
  % TODO promo
  pgn:full_move(P, [move, pawn, Source, _, Dest, Promotion], Pgn).

uci_pgn_move(P, Source - Dest, Pgn) :-
  fen:piece_at(P, Source, [king, Color]),
  fen:turn(P, Color),

  castling_square(Color, Side, Dest),
  pgn:castling_move(Pgn, Side).

castling_square(white, queenside, Sq) :-
  fen:square_codes("c1", Sq).

castling_square(black, queenside, Sq) :-
  fen:square_codes("c8", Sq).

castling_square(white, kingside, Sq) :-
  fen:square_codes("g1", Sq).

castling_square(black, kingside, Sq) :-
  fen:square_codes("g8", Sq).
