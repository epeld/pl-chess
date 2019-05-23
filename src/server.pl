:- module(server, [server/1]).

:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_parameters)).

:- use_module(services/fen_service, [initial_fen_string/1, parse_string/2]).
:- use_module(services/pgn_service, [parse_pgn_string/2, make_pgn_move/3]).


:- http_handler(/, say_hi, []).
:- http_handler('/pgn/move', make_move, []).

server(Port) :-
  http_server(http_dispatch, [port(Port)]).

say_hi(_Request) :-
  format('Content-type: text/plain~n~n'),

  initial_fen_string(S),
  format('~s', [S]).


make_move(Request) :-
  http_parameters(Request,
                  [
                    fen(FenA, []),
                    move(PgnA, [])
                  ]),
  format('Content-type: text/plain~n~n'),

  atom_codes(FenA, Fen),
  atom_codes(PgnA, Pgn),

  or_fail(fen_service:parse_string(Fen, Position), invalid_fen(FenA)),
  or_fail(pgn_service:parse_pgn_string(Pgn, Move), invalid_pgn(PgnA)),
  or_fail(pgn_service:make_pgn_move(Position, Move, Position2), invalid_move(PgnA)),

  fen_service:encode_position(Position2, Fen2),
  format('~s', [Fen2]).

:- meta_predicate
    or_fail(0),
    or_fail(0, +).

or_fail(Goal) :-
  or_fail(Goal, goal_didnt_succeed(Goal)).

or_fail(Goal, Throw) :-
  call(Goal) *-> true
  ; throw(Throw).
