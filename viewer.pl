:- module(viewer, []).

:- set_prolog_flag(double_quotes, codes).

:- use_module(read).
:- use_module(fen).
:- use_module(uci).


initial_state(state(Zipper, Options)) :-
  zsingleton(Zipper, Pos),
  initial_position(Pos),
  initial_options(Options).

initial_position(todo).


repl(State) :-
  thread_get_message(Msg),
  process_message(Msg, State, State2),
  repl(State2).


process_message(Msg, state(Z, Options), state(Z2, Options)) :-
  zipper_message(Msg, Z, Z2).

process_message(Msg, state(Z, Options), state(Z, Options2)) :-
  option_message(Msg, Options, Options2).

%
% Game State Modifiers/Accessors
%
zipper_message(forward, Z, Z2) :-
  zipper2:zforward(Z, Z2).

zipper_message(backward, Z, Z2) :-
  zipper2:zforward(Z2, Z).

zipper_message(move_number(N), Z, Z2) :-
  zipper2:zrewind(Z, Z0),
  zipper_message(forward(N), Z0, Z2).

zipper_message(last, Z, Z2) :- zipper2:zfastforward(Z, Z2).

zipper_message(first, Z, Z2) :- zipper2:zrewind(Z, Z2).


%
%  Options
%
option_message(none, O, O). % TODO
