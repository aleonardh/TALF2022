/* Declaraciones, variables locales (copia directa a lex.yy.c) */
%{

#include <stdio.h>
#include <string.h>
#include "tokens.h"

%}

%option yylineno
%x COMENTARIO_MULTILINEA
/* Definiciones de estados y reglas */




/* Identificadores (empieza por letra o '_') seguido de x */
IDENTIFICADOR [a-zA-Z_][a-zA-Z0-9_]*

/*Operadores de Un solo caracter*/
OPERADORES_UN_SOLO_CAR_ARITMETICOS (?:\*|\/|%|\+|-)
OPERADORES_UN_SOLO_CAR_MEMORIA (?:\.|\*|\&|\[|\])
OPERADORES_UN_SOLO_CAR_BITS (?:\-|\&|\||\^)
OPERADORES_UN_SOLO_CAR_RELACIONALES (?:<|>)
OPERADORES_UN_SOLO_CAR_LOGICOS (?:<|>)
OPERADORES_UN_SOLO_CAR_OTROS [=]

CTC_ENTERA_DECIMAL [0-9]+
CTC_ENTERA_HEX 0[xX][0-9a-fA-F]+
HEX [0-9a-fA-F]+


CTC_REAL_DECIMAL {CTC_ENTERA_DECIMAL}\.{CTC_ENTERA_DECIMAL}
CTC_REAL_HEX {CTC_ENTERA_HEX}\.{CTC_ENTERA_HEX}

CTC_REAL_MANTISA_DECIMAL (?:{CTC_REAL_DECIMAL}|{CTC_ENTERA_DECIMAL})(\*\*)[+-]?(?:{CTC_ENTERA_DECIMAL}|{CTC_REAL_DECIMAL})
CTC_REAL_MANTISA_HEX (?:{CTC_REAL_HEX}|{CTC_ENTERA_HEX})(\*\*)[+-]?(?:{HEX})

/*Caracteres*/
CHAR_UNICO [^'\\\n]
CHAR_ESCAPADOS (?:\\'|\\\"|\\\?|\\\\|\\[nrtv])
CHAR_OCTAL  \\[0-3]?[0-7]{1,2}
CHAR_HEX \\x[0-9a-fA-F]{1,2}

CHAR (?:'{CHAR_UNICO}'|'{CHAR_ESCAPADOS}'|'{CHAR_OCTAL}'|'{CHAR_HEX}')

/*Cadenas*/
/* CADENA (?:\"[^"\\\n]\"|\"{CHAR_ESCAPADOS}\"|\"{CHAR_OCTAL}\"|\"{CHAR_HEX}\"|\"\\\n\")* */
CADENA (?:{CHAR_ESCAPADOS}|{CHAR_OCTAL}|{CHAR_HEX}|(\\)(\n)|[^"\n\\])*
CTC_CADENA \"{CADENA}\"


/*Delimitadores*/
DELIMITADORES (?:\(|\)|\{|\}|\?|:|;|,)


/*Comentarios*/
COMENTARIO \/\/.*




%%
<INITIAL>{
"/*"              BEGIN(COMENTARIO_MULTILINEA);
}
<COMENTARIO_MULTILINEA>{
"*/"      BEGIN(INITIAL);
[^*\n]+   // eat comment in chunks
"*"       // eat the lone star
\n        yylineno++;
}
{COMENTARIO}    ;

(?:\n|[ \t]+)   ;


(?i:ABSTRACT)            {
        fprintf(yyout,"Linea %d, palabra reservada: %s\n",yylineno,yytext);
        return(ABSTRACT);
}
(?i:BASE)            {
        fprintf(yyout,"Linea %d, palabra reservada: %s\n",yylineno,yytext);
        return(BASE);
}
(?i:BOOLEAN)            {
        fprintf(yyout,"Linea %d, palabra reservada: %s\n",yylineno,yytext);
        return(BOOLEAN);
}
(?i:BREAK)  {
        fprintf(yyout,"Linea %d, palabra reservada: %s\n",yylineno,yytext);
        return(BREAK);
}
(?i:CASE)   {
        fprintf(yyout,"Linea %d, palabra reservada: %s\n",yylineno,yytext);
        return(CASE);
}
(?i:CATCH)      {
        fprintf(yyout,"Linea %d, palabra reservada: %s\n",yylineno,yytext);
        return(CATCH);
}
(?i:CHAR)      {
        fprintf(yyout,"Linea %d, palabra reservada: %s\n",yylineno,yytext);
        return(CHAR);
}
(?i:CLASS)      {
        fprintf(yyout,"Linea %d, palabra reservada: %s\n",yylineno,yytext);
        return(CLASS);
}
(?i:CONTINUE)      {
        fprintf(yyout,"Linea %d, palabra reservada: %s\n",yylineno,yytext);
        return(CONTINUE);
}
(?i:DEFAULT)      {
        fprintf(yyout,"Linea %d, palabra reservada: %s\n",yylineno,yytext);
        return(DEFAULT);
}
(?i:DO)      {
        fprintf(yyout,"Linea %d, palabra reservada: %s\n",yylineno,yytext);
        return(DO);
}
(?i:DOUBLE)      {
        fprintf(yyout,"Linea %d, palabra reservada: %s\n",yylineno,yytext);
        return(DOUBLE);
}
(?i:ELSE)      {
        fprintf(yyout,"Linea %d, palabra reservada: %s\n",yylineno,yytext);
        return(ELSE);
}
(?i:ENUM)      {
        fprintf(yyout,"Linea %d, palabra reservada: %s\n",yylineno,yytext);
        return(ENUM);
}
(?i:EXTERN)      {
        fprintf(yyout,"Linea %d, palabra reservada: %s\n",yylineno,yytext);
        return(EXTERN);
}
(?i:FINALLY)      {
        fprintf(yyout,"Linea %d, palabra reservada: %s\n",yylineno,yytext);
        return(FINALLY);
}
(?i:FLOAT)      {
        fprintf(yyout,"Linea %d, palabra reservada: %s\n",yylineno,yytext);
        return(FLOAT);
}
(?i:FOR)      {
        fprintf(yyout,"Linea %d, palabra reservada: %s\n",yylineno,yytext);
        return(FOR);
}
(?i:GOTO)      {
        fprintf(yyout,"Linea %d, palabra reservada: %s\n",yylineno,yytext);
        return(GOTO);
}
(?i:IF)      {
        fprintf(yyout,"Linea %d, palabra reservada: %s\n",yylineno,yytext);
        return(IF);
}
(?i:INT)      {
        fprintf(yyout,"Linea %d, palabra reservada: %s\n",yylineno,yytext);
        return(INT);
}
(?i:INTERFACE)      {
        fprintf(yyout,"Linea %d, palabra reservada: %s\n",yylineno,yytext);
        return(INTERFACE);
}
(?i:INTERNAL)      {
        fprintf(yyout,"Linea %d, palabra reservada: %s\n",yylineno,yytext);
        return(INTERNAL);
}
(?i:LONG)      {
        fprintf(yyout,"Linea %d, palabra reservada: %s\n",yylineno,yytext);
        return(INTERNAL);
}
(?i:NAMESPACE)      {
        fprintf(yyout,"Linea %d, palabra reservada: %s\n",yylineno,yytext);
        return(NAMESPACE);
}
(?i:NEW)      {
        fprintf(yyout,"Linea %d, palabra reservada: %s\n",yylineno,yytext);
        return(NEW);
}
(?i:OVERRIDE)      {
        fprintf(yyout,"Linea %d, palabra reservada: %s\n",yylineno,yytext);
        return(OVERRIDE);
}
(?i:PRIVATE)      {
        fprintf(yyout,"Linea %d, palabra reservada: %s\n",yylineno,yytext);
        return(PRIVATE);
}
(?i:PROTECTED)      {
        fprintf(yyout,"Linea %d, palabra reservada: %s\n",yylineno,yytext);
        return(PROTECTED);
}
(?i:PUBLIC)      {
        fprintf(yyout,"Linea %d, palabra reservada: %s\n",yylineno,yytext);
        return(PUBLIC);
}
(?i:RETURN)      {
        fprintf(yyout,"Linea %d, palabra reservada: %s\n",yylineno,yytext);
        return(RETURN);
}
(?i:SEALED)      {
        fprintf(yyout,"Linea %d, palabra reservada: %s\n",yylineno,yytext);
        return(SEALED);
}
(?i:SHORT)      {
        fprintf(yyout,"Linea %d, palabra reservada: %s\n",yylineno,yytext);
        return(SHORT);
}
(?i:SIGNED)      {
        fprintf(yyout,"Linea %d, palabra reservada: %s\n",yylineno,yytext);
        return(SIGNED);
}
(?i:STATIC)      {
        fprintf(yyout,"Linea %d, palabra reservada: %s\n",yylineno,yytext);
        return(STATIC);
}
(?i:STRUCT)      {
        fprintf(yyout,"Linea %d, palabra reservada: %s\n",yylineno,yytext);
        return(STATIC);
}
(?i:SWITCH)      {
        fprintf(yyout,"Linea %d, palabra reservada: %s\n",yylineno,yytext);
        return(STATIC);
}
(?i:THIS)      {
        fprintf(yyout,"Linea %d, palabra reservada: %s\n",yylineno,yytext);
        return(THIS);
}

(?i:THROW)      {
        fprintf(yyout,"Linea %d, palabra reservada: %s\n",yylineno,yytext);
        return(THROW);
}
(?i:TRY)      {
        fprintf(yyout,"Linea %d, palabra reservada: %s\n",yylineno,yytext);
        return(TRY);
}
(?i:TYPEDEF)      {
        fprintf(yyout,"Linea %d, palabra reservada: %s\n",yylineno,yytext);
        return(TYPEDEF);
}
(?i:UNION)      {
        fprintf(yyout,"Linea %d, palabra reservada: %s\n",yylineno,yytext);
        return(UNION);
}
(?i:UNSIGNED)      {
        fprintf(yyout,"Linea %d, palabra reservada: %s\n",yylineno,yytext);
        return(UNSIGNED);
}
(?i:USING)      {
        fprintf(yyout,"Linea %d, palabra reservada: %s\n",yylineno,yytext);
        return(USING);
}
(?i:VIRTUAL)      {
        fprintf(yyout,"Linea %d, palabra reservada: %s\n",yylineno,yytext);
        return(VIRTUAL);
}
(?i:VOID)      {
        fprintf(yyout,"Linea %d, palabra reservada: %s\n",yylineno,yytext);
        return(VOID);
}
(?i:WHILE)      {
        fprintf(yyout,"Linea %d, palabra reservada: %s\n",yylineno,yytext);
        return(WHILE);
}

(?:\+\+)      {
        fprintf(yyout,"Linea %d, operador: %s\n",yylineno,yytext);
        return(INC);
}
(?:\+\=)      {
        fprintf(yyout,"Linea %d, operador: %s\n",yylineno,yytext);
        return(SUMA_ASIG);
}
(?:%=)      {
        fprintf(yyout,"Linea %d, operador: %s\n",yylineno,yytext);
        return(MOD_ASIG);
}
(?:^=)      {
        fprintf(yyout,"Linea %d, operador: %s\n",yylineno,yytext);
        return(XOR_ASIG);
}
(?:==)      {
        fprintf(yyout,"Linea %d, operador: %s\n",yylineno,yytext);
        return(EQ);
}
(?:->)      {
        fprintf(yyout,"Linea %d, operador: %s\n",yylineno,yytext);
        return(PTR_ACCESO);
}
(?:--)      {
        fprintf(yyout,"Linea %d, operador: %s\n",yylineno,yytext);
        return(DEC);
}
(?:-=)      {
        fprintf(yyout,"Linea %d, operador: %s\n",yylineno,yytext);
        return(RESTA_ASIG);
}
(?:<<=)      {
        fprintf(yyout,"Linea %d, operador: %s\n",yylineno,yytext);
        return(DESPI_ASIG);
}
(?:\|=)      {
        fprintf(yyout,"Linea %d, operador: %s\n",yylineno,yytext);
        return(OR_ASIG);
}
(?:!=)      {
        fprintf(yyout,"Linea %d, operador: %s\n",yylineno,yytext);
        return(NEQ);
}
(?:<<)      {
        fprintf(yyout,"Linea %d, operador: %s\n",yylineno,yytext);
        return(DESPI);
}
(?:\*=)      {
        fprintf(yyout,"Linea %d, operador: %s\n",yylineno,yytext);
        return(MULT_ASIG);
}
(?:>>=)      {
        fprintf(yyout,"Linea %d, operador: %s\n",yylineno,yytext);
        return(MULT_ASIG);
}
(?:>=)      {
        fprintf(yyout,"Linea %d, operador: %s\n",yylineno,yytext);
        return(GE);
}
(?:&&)      {
        fprintf(yyout,"Linea %d, operador: %s\n",yylineno,yytext);
        return(AND);
}
(?:>>)      {
        fprintf(yyout,"Linea %d, operador: %s\n",yylineno,yytext);
        return(DESPD);
}
(?:\/=)      {
        fprintf(yyout,"Linea %d, operador: %s\n",yylineno,yytext);
        return(DIV_ASIG);
}
(?:&=)      {
        fprintf(yyout,"Linea %d, operador: %s\n",yylineno,yytext);
        return(AND_ASIG);
}
(?:<=)      {
        fprintf(yyout,"Linea %d, operador: %s\n",yylineno,yytext);
        return(LE);
}
(?:\|\|)      {
        fprintf(yyout,"Linea %d, operador: %s\n",yylineno,yytext);
        return(OR);
}
(?:SIZEOF)      {
        fprintf(yyout,"Linea %d, operador: %s\n",yylineno,yytext);
        return(SIZEOF);
}
(?:TRUE)      {
        fprintf(yyout,"Linea %d, constante booleana: %s\n",yylineno,yytext);
        return(TRUE);
}
(?:FALSE)      {
        fprintf(yyout,"Linea %d, constante booleana: %s\n",yylineno,yytext);
        return(FALSE);
}


(?:{OPERADORES_UN_SOLO_CAR_ARITMETICOS}|{OPERADORES_UN_SOLO_CAR_MEMORIA}|{OPERADORES_UN_SOLO_CAR_BITS}|{OPERADORES_UN_SOLO_CAR_RELACIONALES}|{OPERADORES_UN_SOLO_CAR_LOGICOS}|{OPERADORES_UN_SOLO_CAR_OTROS})      {
        fprintf(yyout,"Linea %d, operador: %s\n",yylineno,yytext);
        return(yytext[0]);
}


{DELIMITADORES}        {
        fprintf(yyout,"Linea %d, delimitador: %s\n",yylineno,yytext);
        return(yytext[0]);
}

{IDENTIFICADOR}         {
        fprintf(yyout,"Linea %d, identificador: %s\n",yylineno,yytext);
        return(IDENTIFICADOR);
}

(?:{CTC_REAL_DECIMAL}|{CTC_REAL_HEX})         {
        fprintf(yyout,"Linea %d, ctc real: %s\n",yylineno,yytext);
        return(REAL);
}

(?:{CTC_REAL_MANTISA_DECIMAL}|{CTC_REAL_MANTISA_HEX})         {
        fprintf(yyout,"Linea %d, ctc real: %s\n",yylineno,yytext);
        return(REAL);
}

(?:{CTC_ENTERA_DECIMAL}|{CTC_ENTERA_HEX})         {
        fprintf(yyout,"Linea %d, ctc entera: %s\n",yylineno,yytext);
        return(ENTERO);
}
{CHAR}        {
        fprintf(yyout,"Linea %d, caracter: %s\n",yylineno,yytext);
        return(CARACTER);
}

{CTC_CADENA}        {
        fprintf(yyout,"Linea %d, cadena: %s\n",yylineno,yytext);
        return(CADENA);
}

.	fprintf (yyout,"Linea %d, ERROR: %s\n",yylineno,yytext);




%%

int yywrap()      {
    return 1;
}

int main(int argc, char *argv[]) {
    yyin = fopen(argv[1], "r");
    while(yylex());

    printf("Programa terminado!!");
    return 0;
}
