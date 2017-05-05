
Hello World!
============

The idea of this project is to explore and learn how prolog can be used to process chess games.

The intent is not to implement a chess engine, but rather to create utilities to e.g
parse FEN-strings, PGN-moves, to verify move correctness, to generate a list of legal moves
etc.


# Usage

If you want to try some queries, you can load `load.pl` and look around in the source files. 
The predicates are a bit disorganized but there aren't that many files to choose from anyway.

## Command Line Interface

There is also a simple CLI that can be started by `main:repl` (defined in `main.pl`).
There are three basic commands:

- "move xx yy zz", i.e, execute those PGN-moves and print the resulting position as FEN
- "position xx", i.e, reset the current position to "xx"
- "abort", i.e, quit the CLI.

More to come. The CLI maintains a transcript of all commands issued. The idea is to
construct a prolog unit test directly from the transcript in case of an error.


### Building
A standalone program can be built using the makefile:

    make
    
.. which should create an executable "fen" in the project root
