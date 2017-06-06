:- module(engine_state, []).
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
  arg(2, State, id(Id)).

engine_options(State, Options) :-
  arg(3, State, Options).

engine_analysis(State, Analysis) :-
  arg(4, State, analysis(Analysis)).

engine_state_name(State, Name) :-
  functor(State, Name, _Arity).



%
% State
%

initial_state(initializing).

%
% Transitions
%
transition(_In, initialized(EngineString, Id, Options), idle(EngineString, Id, Options, none)).

transition(In, initializing, initialized(EngineString, unknown, []), engine_string(EngineString)) :-
  send_to_engine(In, "uci~n", []).

transition(In,
           idle(A, B, C, _),
           running(A, B, C, analysis(Position, [], unknown)),
           go(Position, Args)) :-
  
  ( Args = [_Arg], S = "go ~s~n"
  ; Args = [], S = "go~n" ),

  send_to_engine(In, "position ~s~n", [Position]),
  send_to_engine(In, S, Args).


transition(_In,
           running(A, B, C, analysis(P, Pvs, _)),
           idle(A, B, C, analysis(P, Pvs, bestmove(Move, Ponder))),
           bestmove(Move, Ponder)).


%
% Engine Output Parsing
%

% "Ping"
process_line(_In, "readyok", S, S). 


% Parse engine string, then transition (to uci init)
process_line(In, EngineString, initializing, S2) :-
  transition(In, initializing, S2, [EngineString]).


% Parse identification string
process_line(_In, IdString, initialized(EngineString, unknown, Options), initialized(EngineString, id(Id), Options)) :-
  phrase(uci:id_string(Id), IdString).


% Parse options
process_line(_In, OptionString, initialized(EngineString, Id, Options), initialized(EngineString, Id, [Option | Options])) :-
  phrase(uci:option(Option), OptionString).


% transition to uci (after printing all options)
process_line(In, "uciok", S, S2) :-
  engine_state_name(S, initialized),
  transition(In, S, S2).


% transition from running to idle
process_line(In, BestMoveString, S, S2) :-
  phrase(uci:bestmove(BestMove), BestMoveString),
  transition(In, S, S2, bestmove(BestMove)).
