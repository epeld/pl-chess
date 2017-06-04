:- module(engine, []).

:- use_module(library(process)).

:- set_prolog_flag(double_quotes, codes).

foo("position startpos
go
info nodes 2 time 2
bestmove a2a3 ponder (none)").

create_thread(Id) :-
  thread_create(engine_main, Id, [alias(engine)]).


create_engine_process(In, Out) :-
  process_create(path(stockfish), [],
                 [ stdout(pipe(Out)),
                   stdin(pipe(In))
                 ]).

engine_main :-
  setup_call_cleanup(
    create_engine_process(In, Out),
    repl(In, Out, initializing),
    
    (
      close(Out),
      close(In)
    )).


repl(In, Out, State) :-
  process_pending_input(In, State, State1),
  process_pending_messages(Out, State1, State2),

  sleep(0.1),

  repl(In, Out, State2).


%
% Handle requests from other threads
%

process_pending_messages(Out, S, S2) :-
  get_message_non_block(Msg),
  !,
  process_message(Out, S, Msg, S1),
  process_pending_messages(Out, S1, S2).

process_pending_messages(_Out, S, S).

process_message(_, S, ping, S) :-
  format("pong~n").

process_message(_, S, state, S) :-
  write(S),
  format("~n").

get_message_non_block(Msg) :-
  thread_self(Self),
  thread_get_message(Self, Msg, [timeout(0)]),

  % Some debugging:
  format("Received Message~n"),
  write(Msg),
  format("~n").


%
% Engine output Polling
%
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
