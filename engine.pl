:- module(engine, []).

:- use_module(library(process)).

:- set_prolog_flag(double_quotes, codes).


setup_call_cleanup(
  process_create(path(stockfish), [],
                 [ stdout(pipe(Out)),
                   stdin(pipe(In))
                 ]),
  repl(In, Out),
  (
    close(Out),
    close(In)
  )).

repl(In, Out, State) :-
  process_pending_input(In, State, State1),
  process_pending_messages(Out, State1, State2),

  format("Sleeping~n"),
  sleep(3),

  repl(In, Out, State2).


process_pending_messages(_Out, S, S).


process_pending_input(In, State, NextState) :-
  read_line(In, Line),
  !,
  process_line(Line, State, State1),
  process_pending_input(In, State1, NextState).

process_pending_input(_In, State, State).

%
% Engine Output Parsing
%
process_line(Authors, initializing, [initialized, Authors, []]).

process_line("readyok", S, S). % "Ping"

process_line("uciok", [initialized, Authors, Options], [idle, Authors, Options]).

process_line(OptionString, [initialized, Authors, Options], [initialized, Authors, [Option | Options]]) :-
  phrase(uci:option(Option), OptionString).


%
% Low-Level I/O
%
read_line(In, [Char | Line]) :-
  get_char_non_block(In, Char),
  !,
  read_line_block(In, Line), !.


get_char_non_block(S, C) :-
  wait_for_input([S], [S], -1),
  get_char(C).


read_line_block(In, Line) :-
  get_char(In, Char),
  !,
  ( Char = '\n',
    !,
    Line = []
  ; Line = [ Char | Rest ],
    read_line_block(In, Rest) ).

%read_line0(_In, []).
