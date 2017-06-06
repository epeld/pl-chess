:- module(engine_messages, []).

:- use_module(engine_state).

%
% Message processing for our engine thread
%
info(S) :- info(S, []).

info(_, _) :- !. % Comment out to get info messages

info(FormatString, Args) :-
  format(FormatString, Args).

process_message(In, [idle | Rest], go(Position, Args), [running | Rest]) :-
  format("analysing ~s~n", [Position]),
  
  format(In, "position ~s~n", [Position]),
  flush_output(In),
  
  format(In, "go ~s~n", [Args]),
  flush_output(In).


process_message(_, S, ping, S) :-
  format("pong~n").

process_message(_, S, authors, S) :-
  S = [_, Authors | _],
  format("~s~n", [Authors]).


% Helper for when I screw up and forget to flush..
process_message(In, S, flush, S) :-
  flush_output(In).


process_message(_, S, state, S) :-
  write(S),
  format("~n").


process_message(_, S, reader_failed(_Reader), S) :-
  info("Reader died.~n"),
  quit.

process_message(_, S, line_read(_Stream, end_of_file), S) :-
  !,
  info("END OF FILE~n"),
  quit.

process_message(In, S, line_read(_Stream, Codes), S2) :-
  info("Engine: \"~s\"~n", [Codes]),
  catch(
    engine_state:process_line(In, Codes, S, S2),
    Err,
    (
      format("Error: '~w' and ~w~n", [Codes, S]),
      format("Error: ~k~n", Err),
      inspect_error(Err)
    )).


process_message(In, S, quit, S) :-
  format(In, "quit~n", []),
  flush_output(In),
  quit.


quit :-
  throw(quit).


inspect_error(error(existence_error(A,B),context(C,D))) :-
  !,
  format("Terminating because of Existence Error ~w ~w ~w ~w", [A, B, C, D]),
  quit.

% For some errors we can try to keep on going
inspect_error(_Err).
