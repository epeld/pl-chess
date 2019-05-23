:- module(server, [server/1]).

:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_parameters)).

:- use_module(services/fen_service, [initial_fen_string/1, parse_string/2]).


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
                    fen(Fen, []),
                    move(Move, [])
                  ]),
  format('Content-type: text/plain~n~n'),

  format('~s ~s', [Fen, Move]).

