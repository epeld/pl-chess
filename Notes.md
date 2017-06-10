
# Notes for me

## Simple TODOs

- Decode uci_moves to pgn_moves
- Display an ASCII chess board! (Like a FEN string but 's_/_\n_g' and without RLE encoding)
- Implement Combo option type parsing

### Refactorings
For when I am bored:

- Refactor [square, Row, Col] -> square(Row, Col)
- Refactor [move, Piece, Source, MoveType, Dest] -> move(PieceType, Source, ..)

## Slightly Harder TODOs

- (Paritally) Parse the uci spec textfile, extracting the descriptions for standard options and engine infos
- Allow editing and SAVING pgnfiles 
- Implement a 'variation stack' and a 'free analysis'-mode (i.e move pieces freely)


## TODOs for Engine Analysis




### Client

Build a user friendly client for talking to the engine

- Start/Stop the engine
- Get engine status
- Get current engine position and bestline (so far)
- Inspect/Set individual engine options

Eventually:

- Display the primary variations in a GUI 

### "Go"

- Parse engine info-strings during analysis.




## TODOs for GUI Development

- Figure out protocol for GUI -> Logic communication

- Display FEN info somewhere, e.g move nr, whose turn, CHECK, etc

- Define a game-frame that can follow a game.

- Allow board to input new moves, both using keyboard AND mouse

- Parse UCI engine output. Add a frame for that
