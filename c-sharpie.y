%{
  #include <stdlib.h>
  #include <stdio.h>
  extern FILE *yyin;
  extern int linea;
  extern int yylineno;


  #define YYDEBUG 1

%}

%token ABSTRACT BASE BOOLEAN BREAK CASE CATCH CHAR CLASS CONTINUE DEFAULT DO DOUBLE ELSE ENUM EXTERN FINALLY
%token FLOAT FOR GOTO IF INT INTERFACE INTERNAL LONG NAMESPACE NEW OVERRIDE PRIVATE PROTECTED PUBLIC RETURN
%token SEALED SHORT SIGNED STATIC STRUCT SWITCH THIS THROW TRY TYPEDEF UNION UNSIGNED USING VIRTUAL VOID
%token WHILE

%token TRUE FALSE SIZEOF IDENTIFICADOR ENTERO CADENA REAL CARACTER PTR_ACCESO INC DEC DESPI DESPD LE GE EQ
%token NEQ AND OR MULT_ASIG DIV_ASIG MOD_ASIG SUMA_ASIG RESTA_ASIG DESPI_ASIG DESPD_ASIG AND_ASIG XOR_ASIG
%token OR_ASIG
%start modulo

%%

/************/
/* PROGRAMA */
/************/

/************/
/*modulo ::= [ directiva_uso ]* [ declaracion ]+  */
/************/

modulo : directiva_uso_opcional declaraciones {printf("modulo \n");}
;

directiva_uso_opcional : {printf("directiva vacía");}
                        | directiva_uso_opcional directiva_uso
;

declaraciones : declaracion
                | declaraciones declaracion
;

/************/
/*declaracion ::= declaracion_espacio_nombres
| declaracion_variable
| declaracion_tipo
| declaracion_funcion
*/
/************/

declaracion : declaracion_espacio_nombres
;

/************/
/*directiva_uso ::= ’using’ [ IDENTIFICADOR ’=’ ]? nombre_tipo_o_espacio_nombres ’;’*/
/************/

/* el [IDENTIFICADOR =] ES OPCIONAL */
directiva_uso : USING nombre_tipo_o_espacio_nombres ';' {printf("dir_uso -> USING ID \n");}
               | USING IDENTIFICADOR '=' nombre_tipo_o_espacio_nombres ';' {printf("dir_uso -> USING ID = nom_tipo_o_esp_noms \n");}
;

/************/
/*nombre_tipo_o_espacio_nombres ::= [ identificador_con_tipos ’.’ ]* identificador_con_tipos*/
/************/
nombre_tipo_o_espacio_nombres :  identificador_con_tipos {printf("nom_tipo_o_esp_noms -> id_tipos \n");}
;

/* uno o más identificadores con tipo , separados por puntos, opcional lo de los nombres_tipo_espacio */
identificador_con_tipos : IDENTIFICADOR {printf("id_tipo \n");}
                         | identificador_con_tipos '.' IDENTIFICADOR {printf("identificador con tipos separado por puntos sin nombres de tipo o espacio");}
                         | identificador_con_tipos '.' IDENTIFICADOR '(' nombre_tipo_o_espacio_nombres_separados_por_coma ')' {printf("identificador con tipos separado por puntos sin nombres de tipo o espacio");}
;

/* uno o más nombres de espacio , separados por comas */
nombre_tipo_o_espacio_nombres_separados_por_coma : nombre_tipo_o_espacio_nombres
                                                  | nombre_tipo_o_espacio_nombres_separados_por_coma ',' nombre_tipo_o_espacio_nombres
;
/************/
/*identificador_con_tipos ::= IDENTIFICADOR [ ’(’ ( nombre_tipo_o_espacio_nombres )+ ’)’ ]?*/
/************/






/*******************/
/* ESPACIO NOMBRES */
/*******************/

/*******************/
/* declaracion_espacio_nombres ::= ’namespace’ identificador_anidado bloque_espacio_nombres */
/*******************/
declaracion_espacio_nombres : NAMESPACE identificador_anidado {printf("declaracion espacio \n");}
;



/*******************/
/* identificador_anidado ::= [ IDENTIFICADOR ’.’ ]* IDENTIFICADOR */
/*******************/

identificador_anidado : IDENTIFICADOR {printf("IDENTIFICADOR DENTRO DE DECLARACIÓN DE ESPACIO NOMBRES \n");}
                       | identificador_anidado '.' IDENTIFICADOR {printf("IDENTIFICADOR DENTRO DE DECLARACIÓN DE ESPACIO NOMBRES, separados por puntos br\n");}
;
/*******************/
/* bloque_espacio_nombres ::= ’{’ [ directiva_uso ]* [ declaracion ]+ ’}’ */
/*******************/

bloque_espacio_nombres : '{' directiva_uso_opcional declaraciones '}' {printf("bloque espacio \n");}
;
/*************/
/* VARIABLES */
/*************/

/*************/
/* declaracion_variable ::= tipo ( nombre )+ ’;’ */
/*************/
declaracion_variable : tipo nombre_separados_por_comas ';'
;



/*************/
/* tipo ::= ’<’ nombre_tipo_o_espacio_nombres ’>’ | tipo_escalar */
/*************/

tipo : '<' nombre_tipo_o_espacio_nombres '>' {printf("Tipo < nombre_tipo>");}
      | tipo_escalar
;


nombre_separados_por_comas : nombre
                             | nombre_separados_por_comas ',' nombre {printf("Nombre separados por comas");}
;

/*************/
/* nombre ::= dato [ ’=’ valor ]? */
/*************/
nombre : dato {printf("dato sin valor");}
        | dato '=' valor {printf("dato con valor");}
;


/*************/
/* tipo_escalar ::= [ signo ]? [ longitud ]? tipo_basico */
/*************/
tipo_escalar : tipo_basico {printf("tipo escalar sin signo ni longitud");}
              | signo tipo_basico {printf("tipo escalar solo signo");}
              | longitud tipo_basico {printf("tipo escalar solo longitud");}
              | signo longitud tipo_basico {printf("tipo escalar con signo y longitud");}
;

/*************/
/* longitud ::= ’short’ | ’long’ */
/*************/
longitud : SHORT {printf("longitud short");}
         | LONG  {printf("longitud long");}
;

/*************/
/* signo ::= ’signed’ | ’unsigned’ */
/*************/
signo : SIGNED {printf("signo signed");}
         | UNSIGNED  {printf("signo unsigned");}
;

/*************/
/* tipo_basico ::= ’char’ | ’int’ | ’float’ | ’double’ | ’boolean’ */
/*************/

tipo_basico : CHAR {printf("tipo basico char \n");}
            | INT {printf("tipo basico INT \n");}
            | FLOAT {printf("tipo basico FLOAT \n");}
            | DOUBLE {printf("tipo basico DOUBLE \n");}
            | BOOLEAN {printf("tipo basico BOOLEAN \n");}
;

/*************/
/* dato ::= [ ’*’ ]* dato_indexado */
/*************/

dato : asterisco_opcional dato_indexado {printf(" Dato indexado\n");}
;

asterisco_opcional :
                    | asterisco_opcional '*'
;

/*************/
/* dato_indexado ::= IDENTIFICADOR [ ’[’ ( expresion )* ’]’ ]* */
/*********/

dato_indexado : IDENTIFICADOR {printf("dato indexado ID");}
               | IDENTIFICADOR '[' expresiones_separadas_por_comas ']' {printf("dato indexado ID con expresiones entre corch ");}
;

expresiones_separadas_por_comas :
                                 | expresiones_separadas_por_comas ',' expresion {printf("expresiones separadas por comas");}
;

/*************/
/* valor ::= expresion| ’[’ ( valor )+ ’]’ */
/*********/

valor : expresion {printf("Valor -> solo expr");}
      | '[' valores_separados_por_comas ']' {printf("Valores separados por comas entre corch ");}
;

valores_separados_por_comas : valor
                             | valores_separados_por_comas ',' valor
;
/*********/
/* TIPOS */
/*********/

/*********/
/* declaracion_tipo */
/*********/
declaracion_tipo : nombramiento_tipo {printf("Declaracion tipo -> nomb_tipo");}
                 | declaracion_struct_union {printf("Declaracion tipo -> declaracion_struct_union");}
                 | declaracion_interfaz {printf("Declaracion tipo -> declaracion_interfaz");}
                 | declaracion_enum {printf("Declaracion tipo -> declaracion_enum");}
                 | declaracion_clase {printf("Declaracion tipo -> declaracion_clase");}
;


/*********/
/* nombramiento_tipo ::= ’typedef’ tipo ID ’;’ */
/*********/
nombramiento_tipo : TYPEDEF tipo IDENTIFICADOR ';'

/*********/
/* declaracion_struct_union ::= [ modificador ]* struct_union [ IDENTIFICADOR ]? ’{’ [ declaracion_campo ]+ ’}’ */
/*********/
declaracion_struct_union : modificadores_opcionales struct_union
                           '{' declaracion_campos  '}'
                           | modificadores_opcionales struct_union IDENTIFICADOR
                           '{' declaracion_campos  '}'
;
/*********/
/* MODIFICADORES OPCIONALES */
/*********/
modificadores_opcionales :
                         | modificadores_opcionales modificador
;
/*********/
/* modificador */
/*********/

modificador : NEW {printf("MOD -> NEW");}
            | PUBLIC {printf("MOD -> PUBLIC");}
            | PROTECTED {printf("MOD -> PUBLIC");}
            | INTERNAL {printf("MOD -> INTERNAL");}
            | PRIVATE {printf("MOD -> PRIVATE");}
            | STATIC {printf("MOD -> STATIC");}
            | VIRTUAL {printf("MOD -> VIRTUAL");}
            | SEALED {printf("MOD -> SEALED");}
            | OVERRIDE {printf("MOD -> OVERRIDE");}
            | ABSTRACT {printf("MOD -> ABSTRACT");}
            | EXTERN {printf("MOD -> EXTERN");}
;
/*********/
/* struct_union ::= ’struct’ | ’union’ */
/*********/
struct_union : STRUCT {printf("struct_union -> STRUCT");}
             | UNION  {printf("struct_union -> UNION");}



/*********/
/* declaracion_campo ::= tipo ( nombre )+ ’;’ | declaracion_struct_union ( nombre )+ ’;’*/
/*********/

/* DEFINICIÓN DE UNO O MÁS DECLARACION_CAMPO */

declaracion_campos : declaracion_campo
                    | declaracion_campos declaracion_campo
;

declaracion_campo : tipo nombres_separados_por_comas ';' {printf("declaracion_campo -> tipo nombre(s)");}
                  | declaracion_struct_union nombres_separados_por_comas ';' {printf("declaracion_campo -> declaracion_struct_union nombre(s)");}
;

/* DEFINICIÓN DE UNO O MÁS NOMBRES_separados_por_comas */

nombres : nombres_separados_por_comas
        | nombres_separados_por_comas ',' nombre
;


/*********/
/* declaracion_interfaz ::= [ modificador ]* ’interface’ IDENTIFICADOR [ herencia ]? cuerpo_interfaz */
/*********/

declaracion_interfaz : modificadores_opcionales INTERFACE IDENTIFICADOR cuerpo_interfaz {printf("declaracion_interfaz sin herencia");}
                     | modificadores_opcionales INTERFACE IDENTIFICADOR herencia cuerpo_interfaz {printf("declaracion_interfaz con herencia");}
;

/*********/
/* herencia ::= ’:’ ( nombre_tipo_o_espacio_nombres )+ */
/*********/
herencia : ':' nombre_tipo_o_espacio_nombres_separados_por_coma {printf("herencia ");}
;

/*********/
/* cuerpo_interfaz ::= ’{’ [ declaracion_metodo_interfaz ]* ’}’ */
/*********/
cuerpo_interfaz : '{' declaracion_metodo_interfaz_op '}' {printf("cuerpo interfaz \n");}
;

declaracion_metodo_interfaz_op :
                                | declaracion_metodo_interfaz_op declaracion_metodo_interfaz
;


/*********/
/* declaracion_metodo_interfaz ::= [ ’new’ ]? firma_funcion ’;’ */
/*********/
declaracion_metodo_interfaz : firma_funcion ';' {printf("declaracion_metodo_interfaz \n");}
                             | NEW firma_funcion ';' {printf("declaracion_metodo_interfaz  con new\n");}



/*********/
/* declaracion_enum ::= [ modificador ]* ’enum’ IDENTIFICADOR [ ’:’ tipo_escalar ]? cuerpo_enum */
/*********/

declaracion_enum : modificadores_opcionales ENUM IDENTIFICADOR cuerpo_enum {printf("declaracion enum sin tipo escalar \n");}
                 | modificadores_opcionales ENUM IDENTIFICADOR ':' tipo_escalar cuerpo_enum {printf("declaracion enum con tipo escalar \n");}

/*********/
/* cuerpo_enum ::= ’{’ ( declaracion_miembro_enum )+ ’}’ */
/*********/

cuerpo_enum : '{' declaracion_miembro_enum_separados_por_puntos '}' {printf("cuerpo_enum");}
;

declaracion_miembro_enum_separados_por_comas : declaracion_miembro_enum
                                               | declaracion_miembro_enum_separados_por_comas ',' declaracion_miembro_enum
;

/*********/
/* declaracion_miembro_enum ::= IDENTIFICADOR [ ’=’ expresion ]?’}’ */
/*********/

declaracion_miembro_enum : IDENTIFICADOR {printf("declaracion_miembro_enum sin expresion");}
                         | IDENTIFICADOR '=' expresion {printf("declaracion_miembro_enum con expresion");}
;

/**********/
/* CLASES */
/**********/

/*********/
/* declaracion_clase ::= [ modificador ]* ’class’ IDENTIFICADOR [ herencia ]? cuerpo_clase */
/*********/

declaracion_clase : modificadores_opcionales_clase CLASS IDENTIFICADOR cuerpo_clase {printf("declaracion_clase sin herencia \n");}
                   | modificadores_opcionales_clase CLASS IDENTIFICADOR herencia cuerpo_clase {printf("declaracion_clase con herencia \n");}
;

modificadores_opcionales_clase :
                                | modificadores_opcionales_clase modificador_clase
;

modificador_clase : NEW {printf("MOD CLASE -> NEW \n");}
                  | PUBLIC {printf("MOD CLASE -> PUBLIC \n");}
                  | PROTECTED {printf("MOD CLASE -> PROTECTED \n");}
                  | INTERNAL {printf("MOD CLASE -> INTERNAL \n");}
                  | PRIVATE {printf("MOD CLASE -> PRIVATE \n");}
                  | ABSTRACT {printf("MOD CLASE -> ABSTRACT \n");}
                  | SEALED {printf("MOD CLASE -> SEALED \n");}
                  | STATIC {printf("MOD CLASE -> STATIC \n");}
;

/*********/
/* cuerpo_clase ::= ’{’ [ declaracion_elemento_clase ]+ ’}’ */
/*********/

cuerpo_clase : '{' declaracion_elemento_clases '}' {printf("cuerpo_clase \n");}
;

declaracion_elemento_clases : declaracion_elemento_clase
                            | declaracion_elemento_clases declaracion_elemento_clase
;

/*********/
/* declaracion_elemento_clase */
/*********/

declaracion_elemento_clase : declaracion_tipo {printf("declaracion tipo \n");}
                            | declaracion_atributo {printf("declaracion atributo \n");}
                            | declaracion_metodo {printf("declaracion metodo \n");}
                            | declaracion_constructor {printf("declaracion constructor \n");}
                            | declaracion_destructor {printf("declaracion destructor \n");}



/*********/
/* declaracion_atributo ::= [ modificador ]* declaracion_variable */
/*********/

declaracion_atributo : modificadores_atributo_op declaracion_variable {printf("declaracion atributo \n");}
;

modificadores_atributo_op :
                          | modificadores_atributo_op modificadores_atributo
;
modificadores_atributo : NEW {printf("MOD CLASE -> NEW \n");}
                      | PUBLIC {printf("MOD ATRIBUTO -> PUBLIC \n");}
                      | PROTECTED {printf("MOD ATRIBUTO -> PROTECTED \n");}
                      | INTERNAL {printf("MOD ATRIBUTO -> INTERNAL \n");}
                      | PRIVATE {printf("MOD ATRIBUTO -> PRIVATE \n");}
                      | STATIC {printf("MOD ATRIBUTO -> STATIC \n");}
                      | READONLY {printf("MOD ATRIBUTO -> READONLY \n");}
                      | VOLATILE {printf("MOD ATRIBUTO -> VOLATILE \n");}
;


/*********/
/* declaracion_metodo ::= [ modificador ]* firma_funcion bloque_instrucciones */
/*********/
declaracion_metodo : modificadores_opcionales firma_funcion bloque_instrucciones {printf("declaracion metodo");}
;

/*********/
/* declaracion_constructor ::= [ modificador ]* cabecera_constructor bloque_instrucciones */
/*********/

declaracion_constructor : modificador_constr_opcional cabecera_constructor bloque_instrucciones {printf("DECLARACION CONSTRUCTOR \n");}
;

modificador_constr_opcional :
                            | modificador_constr_opcional modificador_constr
;

modificador_constr : | PUBLIC {printf("MOD CONSTR -> PUBLIC \n");}
                     | PROTECTED {printf("MOD CONSTR -> PROTECTED \n");}
                     | INTERNAL {printf("MOD CONSTR -> INTERNAL \n");}
                     | PRIVATE {printf("MOD CONSTR -> PRIVATE \n");}
                     | EXTERN {printf("MOD CONSTR -> EXTERN \n");}
;


/*********/
/* cabecera_constructor ::= IDENTIFICADOR parametros  [ inicializador_constructor ]? */
/*********/

cabecera_constructor : IDENTIFICADOR parametros {printf("cabecera constr sin inicializador \n");}
                     | IDENTIFICADOR parametros inicializador_constructor {printf("cabecera constr con inicializador \n");}
;
/*********/
/* inicializador_constructor ::= ’:’ BASE parametros | ’:’ THIS parametros */
/*********/

inicializador_constructor : ':' BASE parametros {printf("INIC CONSTR BASE \n");}
                           | ':' THIS parametros {printf("INIC CONSTR THIS \n");}
;
/*********/
/* declaracion_destructor ::= [ modificador ]* cabecera_destructor bloque_instrucciones */
/*********/

declaracion_destructor : modificadores_opcionales cabecera_destructor bloque_instrucciones {printf("DECLARACION DESTR \n");}
;

/*********/
/* cabecera_destructor ::= ’~’ IDENTIFICADOR ’(’ ’)’ */
/*********/

cabecera_destructor : '~' IDENTIFICADOR '(' ')' {printf("CABECERA DESTR \n");}
;


/*************/
/* FUNCIONES */
/*************/

/*************/
/* declaracion_funcion ::= firma_funcion bloque_instrucciones */
/*************/

declaracion_funcion : firma_funcion bloque_instrucciones {printf("Declaracion funcion \n");}
;

/*************/
/* firma_funcion ::= VOID IDENTIFICADOR parametros| tipo [ ’*’ ]* IDENTIFICADOR parametros */
/*************/

firma_funcion : VOID IDENTIFICADOR parametros {printf("FIRMA -> VOID \n");}
               | tipo asterisco_opcional  IDENTIFICADOR parametros {printf("FIRMA -> tipo asteriscos \n");}
;
/*************/
/* parametros ::= ’(’ [ [ argumentos ’;’ ]* argumentos ]? ’)’ */
/*************/

parametros : '(' ')' {printf("PARAMETROS VACIOS \n");}
            | '(' argumentos_separados_por_punto_coma ')' {printf("PARAMETROS CON ARGS SEPARADOS POR ; \n");}
;

argumentos_separados_por_punto_coma : argumentos
                                    | argumentos_separados_por_punto_coma ';' argumentos
;

/*************/
/* argumentos ::= nombre_tipo ( variable )+ */
/*************/

argumentos : nombre_tipo variables_separadas_por_coma {printf("ARGUMENTOS \n");}
;


variables_separadas_por_coma : variable
                              | variables_separadas_por_coma ',' variable
;


/*************/
/* nombre_tipo ::= tipo [ ’*’ ]* */
/*************/

nombre_tipo : tipo asterisco_opcional {printf("NOMBRE_TIPO \n");}
;

/*************/
/* variable ::= IDENTIFICADOR [ '=' expresion ]? */
/*************/

variable : IDENTIFICADOR {printf("VARIABLE \n");}
         | IDENTIFICADOR '=' expresion {printf("VARIABLE CON EXPRESION \n");}
;

/***************/
/* INSTRUCCIONES */
/***************/
instruccion : bloque_instrucciones  {printf("INSTRUCC -> BLOQUE \n");}
            | instruccion_expresion {printf("INSTRUCC -> INSTR_EXP \n");}
            | instruccion_bifurcacion {printf("INSTRUCC -> BIFURC \n");}
            | instruccion_bucle {printf("INSTRUCC -> BUCLE \n");}
            | instruccion_salto {printf("INSTRUCC -> SALTO \n");}
            | instruccion_destino_salto {printf("INSTRUCC -> DESTINO_SALTO \n");}
            | instruccion_retorno {printf("INSTRUCC -> RETORNO \n");}
            | instruccion_lanzamiento_excepcion {printf("INSTRUCC -> LANZAMIENTO EXC \n");}
            | instruccion_captura_excepcion {printf("INSTRUCC -> CAPT EXC \n");}
            | instruccion_vacia {printf("INSTRUCC -> VACIA \n");}
;

declaracion_op :
               | declaracion_op declaracion
;

instruccion_op :
                | instruccion_op instruccion
;

/***************/
/* bloque_instrucciones ::= ’{’ [ declaracion ]* [ instruccion ]* ’}’ */
/***************/

bloque_instrucciones : '{' declaracion_op instruccion_op '}' {printf(" EMPIEZA BLOQUE INSTR \n");}
;

/***************/
/* instruccion_expresion ::= expresion_funcional ’;’ | asignacion ’;’ */
/***************/

instruccion_expresion : expresion_funcional ';'  {printf(" INSTRUCCION EXPR -> EXPR FUNCIONAL \n");}
                      | asignacion ';' {printf(" INSTRUCCION EXPR -> ASIGNACION \n");}
;


/***************/
/* asignacion ::= expresion_indexada operador_asignacion expresion */
/***************/

asignacion : expresion_indexada operador_asignacion expresion  {printf(" ASIGNACION \n");}
;

/***************/
/* operador_asignacion ::= ’=’ | ’*=’ | ’/=’ | ’%=’ | ’+=’ | ’-=’ | ’<<=’ | ’>>=’ | ’&=’ | ’^=’| ’|=’ */
/***************/
operador_asignacion : '=' {printf("OPERADOR ASIG '=' \n");}
                    |'*=' {printf("OPERADOR ASIG '*=' \n");}
                    | '/=' {printf("OPERADOR ASIG '/=' \n");}
                    | '%=' {printf("OPERADOR ASIG '%=' \n");}
                    | '+=' {printf("OPERADOR ASIG '+=' \n");}
                    | '-=' {printf("OPERADOR ASIG '-=' \n");}
                    | '<<=' {printf("OPERADOR ASIG '<<=' \n");}
                    | '>>=' {printf("OPERADOR ASIG '>>=' \n");}
                    | '&=' {printf("OPERADOR ASIG '&=' \n");}
                    | '^=' {printf("OPERADOR ASIG '^=' \n");}
                    | '|=' {printf("OPERADOR ASIG '|=' \n");}
;


/***************/
/* instruccion_bifurcacion ::= ’if’ ’(’ expresion ’)’ instruccion [ ’else’ instruccion ]? | ’switch’ ’(’ expresion ’)’ ’{’ [ instruccion_caso ]+ ’}’ */
/***************/

instruccion_bifurcacion : IF '(' expresion ')' instruccion {printf("INSTRUCCION BIFUR SOLO IF '=' \n");}
                         | IF '(' expresion ')' instruccion ELSE instruccion {printf("INSTRUCCION BIFUR IF-ELSE '=' \n");}
                         | SWITCH '(' expresion ')' '{' instruccion_casos '}'  {printf("INSTRUCCION BIFUR SWITCH '=' \n");}
;

instruccion_casos : instruccion_caso
                  | instruccion_casos instruccion_caso
;

/***************/
/* instruccion_caso ::= ’case’ expresion ’:’ instruccion | ’default’ ’:’ instruccion */
/***************/

instruccion_caso : CASE expresion ':' instruccion {printf("INSTRUCCION CASO CASE \n");}
                 | DEFAULT ':' instruccion {printf("INSTRUCCION CASO DEFAULT \n");}
;
/***************/
/* instruccion_bucle ::= ’while’ ’(’ expresion ’)’ instruccion | ’do’ instruccion ’while’ ’( expresion ’)’ ’;’| ’for’ ’(’ ( asignacion )* ’;’ expresion ’;’ ( expresion )* ’)’ instruccion*/
/***************/

instruccion_bucle : WHILE '(' expresion ')' instruccion {printf("INSTRUCCION BUCLE WHILE \n");}
                 | DO instruccion WHILE '(' expresion ')' ';' {printf("INSTRUCCION BUCLE DO-WHILE \n");}
                 | FOR '(' asignaciones_opcionales_separadas_por_coma ';' expresion ';' expresiones_opcionales_por_coma ')' instruccion {printf("INSTRUCCION BUCLE DO \n");}
;

asignaciones_opcionales_separadas_por_coma :
                                            | asignaciones_opcionales_separadas_por_coma ',' asignacion
;
expresiones_opcionales_por_coma :
                                            | expresiones_opcionales_por_coma ',' expresion
;

/***************/
/* instruccion_salto ::= ’goto’ IDENTIFICADOR ’;’ | ’continue’ ’;’ | ’break’ ’;’ */
/***************/

instruccion_salto : GOTO IDENTIFICADOR ';' {printf("INSTRUCCION SALTO GOTO \n");}
                    | CONTINUE ';'  {printf("INSTRUCCION SALTO CONTINUE \n");}
                    | BREAK ';'  {printf("INSTRUCCION SALTO BREAK \n");}
;

/***************/
/* instruccion_retorno ::= ’return’ [ expresion ]? ’;’ */
/***************/

instruccion_retorno : RETURN ';' {printf("INSTRUCCION RETORNO \n");}
                    | RETURN expresion ';' {printf("INSTRUCCION RETORNO EXPR \n");}
;

/***************/
/* instruccion_lanzamiento_excepcion ::= ’throw’ expresion ’;’ */
/***************/

instruccion_lanzamiento_excepcion : THROW expresion ';' {printf("INSTRUCCION LANZ EXCEPCION \n");}
;
/***************/
/* instruccion_captura_excepcion ::= ’try’ bloque_instrucciones clausulas_catch | ’try’ bloque_instrucciones [ clausulas_catch ]? clausula_finally*/
/***************/

instruccion_captura_excepcion : TRY bloque_instrucciones clausulas_catch {printf("INSTRUCCION CAPT EXC \n");}
                              | TRY bloque_instrucciones clausula_finally {printf("INSTRUCCION CAPT EXC FINALLY\n");}
                              | TRY bloque_instrucciones clausulas_catch clausula_finally {printf("INSTRUCCION CAPT EXC FINALLY  \n");}
;

/***************/
/* clausulas_catch ::= [ clausula_catch_especifica ]+ | [ clausula_catch_especifica ]* clausula_catch_general*/
/***************/

clausulas_catch : clausulas_catch_especificas {printf("CLAUSULAS CATCH \n");}
                | clausulas_catch_especifica_op clausula_catch_general {printf("CLAUSULAS CATCH ESPEC CON GENERAL \n");}
;
clausulas_catch_especificas : clausulas_catch_especifica
                            | clausulas_catch_especificas clausulas_catch_especifica
;

clausulas_catch_especifica_op :
                              | clausulas_catch_especifica_op clausulas_catch_especifica
;

/***************/
/* clausula_catch_especifica ::= ’catch’ ’(’ nombre_tipo ’)’ bloque_instrucciones */
/***************/
clausula_catch_especifica : CATCH '(' nombre_tipo ')' bloque_instrucciones
;

/***************/
/* clausula_catch_general ::= ’catch’ bloque_instrucciones */
/***************/
clausula_catch_general : CATCH bloque_instrucciones
;

/***************/
/* clausula_finally ::= ’finally’ bloque_instrucciones */
/***************/
clausula_finally : FINALLY bloque_instrucciones
;

/***************/
/* instruccion_vacia ::= ';' */
/***************/
instruccion_vacia : ';'
;


/***************/
/* EXPRESIONES */
/***************/

%%

int yyerror(char *s) {
  fflush(stdout);
  printf("Error linea %d, %s\n", yylineno,s);
  }

int yywrap() {
  return 1;
  }

int main(int argc, char *argv[]) {

  yydebug = 0;

  if (argc < 2) {
    printf("Uso: ./c-sharpie NombreArchivo\n");
    }
  else {
    yyin = fopen(argv[1],"r");
    yyparse();
    }
  }
