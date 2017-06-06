
# Notes for me

## Simple TODOs

- Parse engine info strings (don't assume the order of the params in it!)
- Decode uci_moves to pgn_moves
- Display an ASCII chess board!
- Fix bug in pgnfile parsing (see sample.pgn)
- Implement Combo option type parsing

## Slightly Harder TODOs

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
