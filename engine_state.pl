:- module(engine_state, []).

:- set_prolog_flag(double_quotes, codes).

%
% Commands to send to the engine
%
stop(EngineInput) :-
  send_to_engine(EngineInput, "stop~n", []).

quit(EngineInput) :-
  send_to_engine(EngineInput, "quit~n", []).

%
% This module keeps track of UCI engine state and how to transition between those states
%

send_to_engine(EngineInput, String, Args) :-
  format(EngineInput, String, Args),
  flush_output(EngineInput).


%
% Accessors
%
engine_string(State, String) :-
  arg(1, State, engine_string(String)).

engine_id(State, Id) :-
  arg(2, State, Id).

engine_options(State, Options) :-
  arg(3, State, Options).

engine_analysis(State, Analysis) :-
  arg(4, State, Analysis).

engine_state_name(State, Name) :-
  functor(State, Name, _Arity).


analysis_bestmove(Analysis, BestMove) :-
  arg(3, Analysis, BestMove).

analysis_infos(Analysis, Infos) :-
  arg(2, Analysis, Infos).

analysis_position(Analysis, Position) :-
  arg(1, Analysis, Position).

%
% State
%

initial_state(initializing).

%
% State Transitions
%
transition(_In, initialized(EngineString, Id, Options), idle(EngineString, Id, Options, none), uciok).

transition(In, initializing, initialized(EngineString, [], []), engine_string(EngineString)) :-
  send_to_engine(In, "uci~n", []).

% TODO!
%transition(In, S, S2,
%           setoption(Name, Value)).

transition(In,
           idle(A, B, C, _),
           running(A, B, C, analysis(Position, [], unknown)),
           go(Position, Args)) :-
  
  ( Args = [_Arg], S = "go ~s~n"
  ; Args = [], S = "go~n" ),

  send_to_engine(In, "position ~s~n", [Position]),
  send_to_engine(In, S, Args).


transition(In, S, S2, go(Position)) :-
  transition(In, S, S2, go(Position, [])).


transition(_In,
           running(A, B, C, analysis(P, Infos, _)),
           idle(A, B, C, analysis(P, Infos, bestmove(Move, Ponder))),
           bestmove(Move, Ponder)).


%
% Engine Output Parsing
%

% "Ping"
process_line(_In, "readyok", S, S). 


% Parse engine string, then transition (to uci init)
process_line(In, EngineString, initializing, S2) :-
  transition(In, initializing, S2, engine_string(EngineString)).


% Parse identification string
process_line(_In, IdString,
             initialized(EngineString, Ids, Options),
             initialized(EngineString, [Id | Ids], Options)) :-
  
  phrase(uci:id_string(Id), IdString).

% a blank line is sent after identification info
process_line(_In, "",
             initialized(A, B, C),
             initialized(A, B, C)).


% Parse options
process_line(_In, OptionString,
             initialized(EngineString, Id, Options),
             initialized(EngineString, Id, [Option | Options])) :-
  
  phrase(uci:option(Option), OptionString).


% transition to uci (after printing all options)
process_line(In, "uciok", S, S2) :-
  transition(In, S, S2, uciok).

% transition from running to idle
process_line(In, BestMoveString, S, S2) :-
  phrase(uci:bestmove(BestMove), BestMoveString),
  transition(In, S, S2, BestMove).


% "info .."
process_line(_In, InfoString, S, S2) :-

  phrase(uci:info_line(Infos), InfoString),
  append_infos(Infos, S, S2).


%
% Helpers
%

append_infos(Infos,
             running(EngineString, Id, Options, analysis(P, Is, BestMove)),
             running(EngineString, Id, Options, analysis(P, [Infos | Is], BestMove))) :-
  member(multipv(_), Infos).
