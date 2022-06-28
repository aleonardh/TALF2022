# Fichero flex a compilar (sin extensión)
SOURCE=LeonardHerreraRodriguezVillalta

$(SOURCE): lex.yy.c	
	gcc -o $@ $^ -lfl

lex.yy.c: $(SOURCE).l
	flex $<

clean:
	rm lex.yy.c
