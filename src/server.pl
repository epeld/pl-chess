:- module(server, [server/1]).

:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_parameters)).
:- use_module(library(http/html_write)).

:- use_module(services/fen_service, [initial_fen_string/1, parse_string/2]).
:- use_module(services/pgn_service, [parse_pgn_string/2, make_pgn_move/3]).
:- use_module(services/session_service, [new_session/1]).


:- http_handler(/, say_hi, []).

:- http_handler('/gui', make_gui, []).

:- http_handler('/pgn/move', make_move, []).
:- http_handler('/pgn/squares', possible_squares, []).

:- http_handler('/sessions', handle_sessions_request(Method), [method(Method)]).
:- http_handler(root(sessions / SessionId), handle_sessions_request(delete, SessionId), [method(delete)]).

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


possible_squares(Request) :-
  http_parameters(Request,
                  [
                    fen(FenA, []),
                    source(SquareA, [])
                  ]),
  format('Content-type: text/plain~n~n'),

  atom_codes(SquareA, SquareS),
  atom_codes(FenA, FenS),

  or_fail(fen_service:parse_string(FenS, Position), invalid_fen(FenA)),
  or_fail(fen_service:parse_square(SquareS, Square), invalid_square(SquareA)),

  pgn_service:find_possible_destinations(Position, Square, Destinations),

  maplist(fen_service:encode_square, Destinations, DestinationStrings),
  foreach(
    member(S, DestinationStrings),
    format('~s~n', [S])
  ).

handle_sessions_request(post, _Request) :-
  session_service:new_session(SessionId),
  format('Content-type: text/plain~n~n'),
  format('~s', [SessionId]).

handle_sessions_request(get, _Request) :-
  session_service:find_all_sessions(SessionIds),
  format('Content-type: text/plain~n~n'),
  foreach(
    member(SessionId, SessionIds),
    format('~s~n', [SessionId])
  ).

handle_sessions_request(delete, SessionId, _Request) :-
  session_service:delete_session(SessionId).


make_gui(_Request) :-
  length(Rows, 8),
  maplist(square_row, Rows, [white, black, white, black, white, black, white, black]),
  append(Rows, Squares),
  html_write:reply_html_page(
    title('The Chess GUI'),
    [
      p('Hello World'),
      style('.board { background: purple; height: 300px; width:300px }'),
      style('.board { display: flex; align-items: stretch; flex-direction: row; justify-content: space-between; flex-wrap: wrap }'),
      style('.white { background: gray; width: 12.5%; height: 12.5%; }'),
      style('.black { background: black; width: 12.5%; height: 12.5%; }'),
      div(class(board), Squares)
    ]
  ).

square_row(Row, white) :-
  length(Row, 8),
  maplist(make_square, Row, [white, black, white, black, white, black, white, black]).


square_row(Row, black) :-
  length(Row, 8),
  maplist(make_square, Row, [black, white, black, white, black, white, black, white]).

make_square(div([class(Color)],''), Color).

%
% Error Handling
%

:- meta_predicate
    or_fail(0),
    or_fail(0, +).

or_fail(Goal) :-
  or_fail(Goal, goal_didnt_succeed(Goal)).

or_fail(Goal, Throw) :-
  call(Goal) *-> true
  ; throw(Throw).
