
# Notes for me

## Simple TODOs

- Work on pgnfile parsing/encoding, not creating so many choice points (encode checks!)
- Display an ASCII chess board! (Like a FEN string but 's_/_\n_g' and without RLE encoding)
- Implement 'setoption' state transition (find option in list, replace it)

### Refactorings
For when I am bored:

- Refactor [move, Piece, Source, MoveType, Dest] -> move(PieceType, Source, ..)

## Slightly Harder TODOs

- Implement a 'game viewer' REPL in a separate thread (just like the engine thread)
- Allow changing options in GUI


### "Nice to haves"

- (Paritally) Parse the uci spec textfile, extracting the descriptions for standard options and engine infos
- Allow editing and SAVING pgnfiles 
- For the CLI: Implement a 'variation stack' and a 'free analysis'-mode (i.e move pieces freely)


## TODOs for Engine Analysis

- Integrate with CLI
- Print a nice summary of stats / pvs / bestmove
- Support more options e.g "go args"
- Print engine options nicely

## TODOs for GUI Development

- Display the primary variations in a GUI 
- Display options in a GUI
