:- module(session_service,
          [
            new_session/1,
            delete_session/1
            session/1,
            session/2
          ]).
:- use_module(library(random)).

:- set_prolog_flag(double_quotes, codes).

%
% Public API
%

:- dynamic session/2.

session(SessionId) :-
  session(SessionId, _).

new_session(SessionId) :-
  uuid(SessionId),
  with_mutex(
    session,
    store_session(SessionId)
  ).

delete_session(SessionId) :-
  with_mutex(
    session,
    retract(session(SessionId, _))
  ).

%
% Private API
%

store_session(SessionId) :-
  \+ session(SessionId),
  InitialFEN = "TODO",
  assertz(session(SessionId, InitialFEN)).



uuid(Id) :-
  length(IdC, 10),
  maplist(random_element, IdC),
  atom_codes(Id, IdC).

random_element(Element) :-
  random_member(Element, "0123456789abcdefghijklmnopqrstuvwxyz").
