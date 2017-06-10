:- module(engine_messages, []).

:- use_module(engine_state).

%
% Message processing for our engine thread
%

% synchronous call
process_message(In, S, reply_to(From, Req), S2) :-
  info("Sync message ~w~n", [Req]),
  sync(Req, In, S, S2, Rep),
  thread_send_message(From, Rep).

% asynchronous call
process_message(In, S, async(Req), S2) :-
  info("Async message ~w~n", [Req]),
  async(Req, In, S, S2).

% Reader process failure
process_message(_In, S, reader_failed(_Reader), S) :-
  info("Reader died.~n"),
  quit.

% Engine process failure
process_message(_, S, line_read(_Stream, end_of_file), S) :-
  info("Engine died.~n"),
  quit.

% Engine process output
process_message(In, S, line_read(_Stream, Codes), S2) :-
  compound(Codes),
  info("Engine: \"~s\"~n", [Codes]),
  catch(
    engine_state:process_line(In, Codes, S, S2),
    Err,
    (
      format("Error: '~w' and ~w~n", [Codes, S]),
      format("Error: ~k~n", Err),
      inspect_error(Err)
    )).

inspect_error(error(existence_error(A,B),context(C,D))) :-
  !,
  format("Terminating because of Existence Error ~w ~w ~w ~w", [A, B, C, D]),
  quit.

% For some errors we can try to keep on going
inspect_error(_Err).

% We quit by escalating an exception to the top of the engine REPL
quit :-
  throw(quit).

%
% Engine Messages
%
async(go(Position, Options), In, S, S2) :-
  engine_state:transition(In, S, S2, go(Position, Options)).

async(quit, In, S, S) :-
  engine_state:quit(In),
  quit.

async(stop, In, S, S) :-
  engine_state:stop(In).

async(setoption(Name, Value), In, S, S2) :-
  engine_state:transition(In, S, S2, setoption(Name, Value)).


sync(state, _In, S, S, ok).

sync(ping, _, _, _, ok).


%
% Debug output helper:
%
info(S) :- info(S, []).

info(_, _) :- !. % Comment out to get info messages

info(FormatString, Args) :-
  format(FormatString, Args).
