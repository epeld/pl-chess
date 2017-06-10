
# Notes for me

## Simple TODOs

- Write a util converting PGN moves to their 'shortest unambigous form' (i.e Nce2 -> Ne2 when possible)
- Display an ASCII chess board! (Like a FEN string but 's_/_\n_g' and without RLE encoding)
- Implement Combo option type parsing

### Refactorings
For when I am bored:

- Refactor [square, Row, Col] -> square(Row, Col)
- Refactor [move, Piece, Source, MoveType, Dest] -> move(PieceType, Source, ..)

## Slightly Harder TODOs

- Implement 'setoption' state transition
- (Paritally) Parse the uci spec textfile, extracting the descriptions for standard options and engine infos
- Allow editing and SAVING pgnfiles 
- For the CLI: Implement a 'variation stack' and a 'free analysis'-mode (i.e move pieces freely)
- Display the primary variations in a GUI 


## TODOs for Engine Analysis

- Integrate with CLI
- Print a nice summary of stats / pvs / bestmove
- Support more options e.g "go args"
- Print engine options nicely

## TODOs for GUI Development

- Figure out protocol for GUI -> Logic communication

- Display FEN info somewhere, e.g move nr, whose turn, CHECK, etc

- Define a game-frame that can follow a game.

- Allow board to input new moves, both using keyboard AND mouse

- Parse UCI engine output. Add a frame for that
