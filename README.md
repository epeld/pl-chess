
Hello World!
============

The idea of this project is to explore and learn how prolog can be used to process chess games.

The intent is not to implement a chess engine, but rather to create utilities to e.g
parse FEN-strings, PGN-moves, to verify move correctness, to generate a list of legal moves
etc.

# Requirements

This project works well with SWI-Prolog 6.6.4. Note that for newer versions you might need to configure your system to interpret strings as lists of character codes.

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


# TODOs

## PGN File Parsing

The CLI can be extended with the ability to edit/process PGN files. If we represent the game as a tree,
the user can be allowed to edit and prune variations as well as add comments.

## CLI Options

The CLI can be extended with options for how to represent moves and how much it should print.
This can be useful if we later decide to create a GUI frontend, in which case we should simplify
the output for easier parsing.

## Auto-generated Test Cases

The CLI should get a command for converting the current state into a test case that can later be
fixed. This allows for quicker bug fix progress as well as being a really cool feature.

## GUI

A GUI could communicate with the CLI easily, using e.g the CLI's stdout and stdin to issue commands.
Potential technologies for use here are: Java (Swing), Javascript (e.g angular/react web UI) or Prolog.

## Chess Game Database Procedures

A very useful feature is the ability to search through vast amounts of games stored in database,
 looking for e.g a specific position or something similar (Scid is an example of a chess database
 that can do this). It would be nice to extend the CLI with the ability to query Scids database
 files for a given position.


# Sample Session

If you want to see the 'interpreter' in action:

?- main:repl.
    Position: rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1

	> move e4
	e2e4
	Position: rnbqkbnr/pppppppp/8/8/4P3/8/PPPP1PPP/RNBQKBNR b KQkq e3 0 1
	
	> move d5 Bb5
	d7d5
	Bf1b5+
	Position: rnbqkbnr/ppp1pppp/8/1B1p4/4P3/8/PPPP1PPP/RNBQK1NR b KQkq - 1 2
	CHECK
	> move d6 a3
	Error! Something went wrong. 
	Position: rnbqkbnr/ppp1pppp/8/1B1p4/4P3/8/PPPP1PPP/RNBQK1NR b KQkq - 1 2
	CHECK
	> move Qd7 a3
	Qd8d7
	a2a3
	Position: rnb1kbnr/pppqpppp/8/1B1p4/4P3/P7/1PPP1PPP/RNBQK1NR b KQkq - 0 3
