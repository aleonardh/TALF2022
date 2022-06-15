/* Declaraciones, variables locales (copia directa a lex.yy.c) */
%{

#include <stdio.h>
#include <string.h>
#include "tokens.h"

int i = 1;

%}
/* Definiciones de estados y reglas */



/* Palabras reservadas  */

RESERVADA (?:iabstract|base|boolean|break|case|catch|char|class|continue|default|do|double|else|enum|extern|finally|float|for|goto|if|int|interface|internal|long|namespace|new|override|private|protected|public|return|sealed|short|signed|static|struct|switch|this|throw|try|typedef|union|unsigned|using|virtual|void|while)

/* Identificadores (empieza por letra o '_') seguido de x */
IDENTIFICADOR [a-zA-Z_][a-zA-Z0-9_]*

/* Operadores aritméticos (está hecho con conjuntos, y primero los más largos (++,--,etc)) */
OPERADORES_ARITMETICOS (?:i\+\+|--|\*=|\/=|%=|-=|\+=|\*|\/|%|\+|-)

%%

^{RESERVADA}				fprintf(yyout,"Linea %d, palabra reservada: %s\n",i,yytext);
{RESERVADA}					fprintf(yyout,"Linea %d, palabra reservada: %s\n",i,yytext);
{IDENTIFICADOR}

\n                              i++;
%%

int main(int argc, char *argv[]) {

    yyin = fopen(argv[1], "r");
    while(yylex()){
       yylex();
    }
    //yylex(); //todo while ??
    printf("Programa terminado!!");
