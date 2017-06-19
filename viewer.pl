:- module(viewer, []).

:- set_prolog_flag(double_quotes, codes).

:- use_module(read).
:- use_module(fen).
:- use_module(uci).


initial_state(state(Zipper, Options)).


repl(State) :-
  thread_get_message(Msg),
  process_message(Msg, State, State2),
  repl(State2).


%
% Game State Modifiers/Accessors
%
process_message(forward,
                state(Z, Options),
                state(Z2, Options)) :-
  
  zipper2:zforward(Z, Z2).


process_message(backward,
                state(Z, Options),
                state(Z2, Options)) :-

  zipper2:zforward(Z2, Z).


process_message(move_number(N),
                Z, Z2) :-

  zipper2:zrewind(Z, Z0),
  process_message(forward(N), Z0, Z2).


process_message(last, Z, Z2) :-
  zipper2:zfastforward(Z, Z2).


process_message(first, Z, Z2) :-
  zipper2:zrewind(Z, Z2).


%
%  Options
%
