
.PHONY : clean


fen : *.pl
	swipl -g 'qsave_program("fen", [goal(main:repl)]), halt' load.pl

clean :
	rm fen
