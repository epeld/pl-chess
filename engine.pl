:- module(engine, []).

:- use_module(library(process)).

:- set_prolog_flag(double_quotes, codes).

:- use_module(read).
:- use_module(fen).
:- use_module(uci).

foo("position startpos
go
info nodes 2 time 2
bestmove a2a3 ponder (none)").

%
% Thread helpers
%
cleanup_threads :-
  member(Status, [false, true, exception(_1), exited(_2)]),
  foreach(thread_property(T, status(Status)),
          thread_join(T, Status)),
  fail.

list_threads(Threads) :-
  bagof([T,S], thread_property(T, status(S)), Threads).
                          

create_thread(Id) :-
  (cleanup_threads ; true),
  thread_create(engine_main, Id, [alias(engine)]).


%
% Process
%
create_engine_process(PID, In, Out) :-
  process_create(path(stockfish), [],
                 [ stdout(pipe(Out)),
                   stdin(pipe(In)),
                   process(PID)
                 ]).

%
% REPL
%
engine_main :-
  setup_call_cleanup(
    create_engine_process(PID, In, Out),
    (
      read:start_reader([Out], [alias(reader)], _RId),
      repl(In, initializing)
    ),
    
    (
      process_kill(PID),
      close(Out),
      close(In),
      format("Engine Thread Terminated~n")
    )).

repl(In, State) :-
  process_pending_messages(In, State, State2),
  repl(In, State2).


%
% Handle requests from other threads
%

process_pending_messages(In, S, S2) :-
  thread_get_message(Msg),

  print_message(Msg),
  
  catch(handle_message(In, S, Msg, S2),
        Err,
        (
          format("Error!~n"),
          format("Exception occured processing ~k ~n", [Msg]),
          format("~k~n", [Err]),
          fail
        )).


handle_message(In, S, Msg, S2) :-
  process_message(In, S, Msg, S2),
  format("OK~n"),
  !.

handle_message(_In, S, Msg, S2) :-
  format("Warning: Could not handle message '~k', received in state: ~n", [Msg]),
  write(S),
  format("~n"),
  S = S2.



process_message(_, S, ping, S) :-
  format("pong~n").

process_message(_, S, authors, S) :-
  S = [_, Authors | _],
  format("~s~n", [Authors]).


process_message(In, S, flush, S) :-
  flush_output(In).


process_message(_, S, state, S) :-
  write(S),
  format("~n").

process_message(_, S, line_read(_Stream, end_of_file), S) :-
  !,
  format("END OF FILE~n"),
  fail.

process_message(In, S, line_read(_Stream, Codes), S2) :-
  format("Engine: '~s'~n", [Codes]),
  catch(
    process_line(In, Codes, S, S2),
    Err,
    (
      format("Error: '~s' and ~k~n", Codes, S),
      format("Error: ~k~n", Err)
    )).


process_message(In, S, quit, S) :-
  format(In, "quit~n", []),
  flush_output(In).



print_message(_) :-
  % format("Received message: ~n"),
  % write(Msg),
  % format("~n").
  true.


%
% Engine Output Parsing
%
process_line(In, Authors, initializing, [initialized, Authors, []]) :-
  format(In, "uci~n", []),
  flush_output(In).

process_line(_In, "readyok", S, S). % "Ping"

process_line(_In, "uciok", [initialized, Authors, Options], [idle, Authors, Options]).

process_line(_In, OptionString, [initialized, Authors, Options], [initialized, Authors, [Option | Options]]) :-
  format("Parsing option ~s~n", [OptionString]),
  phrase(uci:option(Option), OptionString).
