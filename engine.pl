:- module(engine, []).

:- use_module(library(process)).

:- set_prolog_flag(double_quotes, codes).

:- use_module(read).
:- use_module(fen).
:- use_module(uci).
:- use_module(thread_tools).

:- use_module(engine_state).
:- use_module(engine_messages).
                     

create_thread(Id, Alias) :-
  (cleanup_threads ; true),
  thread_create(engine_main, Id, [alias(Alias)]).


create_thread(Id) :-
  create_thread(Id, engine).


create_engine_process(PID, In, Out) :-
  create_piped_process(path(stockfish), PID, In, Out).


engine_main :-
  engine_state:initial_state(State),
  
  setup_call_cleanup(
    create_engine_process(PID, In, Out),
    (
      read:start_reader([Out], [alias(reader)], _RId),
      repl(In, State)
    ),
    
    (
      process_kill(PID),
      close(Out),
      close(In),
      format("Engine Thread Terminated~n")
    )).


%
% REPL (really more like "REL")
%
repl(In, State) :-
  handle_pending_message(In, State, State2),
  repl(In, State2).


%
% Handle requests from other threads
%

handle_pending_message(In, S, S2) :-
  thread_get_message(Msg),

  print_message(Msg),
  
  catch(handle_message(In, S, Msg, S2),
        Err,
        handle_error(Err, Msg, S)).


handle_message(In, S, Msg, S2) :-
  engine_messages:process_message(In, S, Msg, S2),
  format("OK~n"),
  !.

handle_message(_In, S, Msg, S2) :-
  format("Warning: Could not handle message '~k', received in state: ~n", [Msg]),
  write(S),
  format("~n"),
  S = S2.


% Comment this out to print engine messages:
print_message(_) :-
  !.

print_message(Msg) :-
  format("Received message: ~n"),
  write(Msg),
  format("~n").



handle_error(Err, Msg, S) :-
  format("Error!~n"),
  format("Exception occured processing ~k ~n", [Msg]),
  format("~k~n", [Err]),
  format("~w~n", [S]),
  thread_send_message(main, engine_error(Err, Msg, S)),
  fail.
        
