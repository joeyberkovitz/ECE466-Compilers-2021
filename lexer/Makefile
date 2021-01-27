CC=gcc
LEX=flex

lexer: lexer.l
	flex -o build/lex.yy.c lexer.l
	gcc -o build/lexer -I. build/lex.yy.c
	
clean:
	rm build/*
	