c-sharpie:	c-sharpie.tab.c c-sharpie.lex.c
	gcc -o c-sharpie c-sharpie.tab.c lex.yy.c -lm
c-sharpie.tab.c:	c-sharpie.y
	bison -dv c-sharpie.y
c-sharpie.lex.c:	c-sharpie.l
	flex c-sharpie.l
clean:
	rm  c-sharpie.tab.c c-sharpie.tab.h c-sharpie.output lex.yy.c c-sharpie
