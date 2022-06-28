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

%%

/************/
/*modulo ::= [ directiva_uso ]* [ declaracion ]+  */
/************/

modulo : directiva_uso_opcional declaraciones {printf("modulo \n");}
;

directiva_uso_opcional : {printf("directiva vacía\n");}
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

declaracion : declaracion_espacio_nombres {printf("DECL -> ESPACIO_NOMBRES \n");}
             | declaracion_variable {printf("DECL -> VARIABLES \n");}
             | declaracion_tipo {printf("DECL -> DECLARACION_TIPO \n");}
             | declaracion_funcion {printf("DECL -> DECLARACION_FUNCION \n");}
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
nombre_tipo_o_espacio_nombres :  identificador_con_tipos_separados_por_puntos  {printf("nom_tipo_o_esp_noms -> id_tipos \n");}
;


/* uno o más identificadores con tipo , separados por puntos, opcional lo de los nombres_tipo_espacio */
identificador_con_tipos_separados_por_puntos : identificador_con_tipos {printf("id_tipo \n");}
                         | identificador_con_tipos_separados_por_puntos '.' identificador_con_tipos {printf("identificador con tipos separado por puntos sin nombres de tipo o espacio\n");}
;


/* uno o más nombres de espacio , separados por comas */
nombre_tipo_o_espacio_nombres_separados_por_coma : nombre_tipo_o_espacio_nombres
                                                  | nombre_tipo_o_espacio_nombres_separados_por_coma ',' nombre_tipo_o_espacio_nombres
;
/************/
/*identificador_con_tipos ::= IDENTIFICADOR [ ’(’ ( nombre_tipo_o_espacio_nombres )+ ’)’ ]?*/
/************/


identificador_con_tipos : IDENTIFICADOR {printf("ID_TIPOS -> ID \n");}
                        |  IDENTIFICADOR '(' nombre_tipo_o_espacio_nombres_separados_por_coma ')' {printf("ID_TIPOS -> ID CON NOMBRES\n");}
;

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

tipo : '<' nombre_tipo_o_espacio_nombres '>' {printf("Tipo < nombre_tipo>\n");}
      | tipo_escalar  {printf("Tipo -> tipo_escalar\n");}
;


nombre_separados_por_comas : nombre
                             | nombre_separados_por_comas ',' nombre {printf("Nombre separados por comas\n");}
;

/*************/
/* nombre ::= dato [ ’=’ valor ]? */
/*************/
nombre : dato {printf("dato sin valor\n");}
        | dato '=' valor {printf("dato con valor\n");}
;


/*************/
/* tipo_escalar ::= [ signo ]? [ longitud ]? tipo_basico */
/*************/
tipo_escalar : tipo_basico {printf("tipo escalar sin signo ni longitud \n");}
              | signo tipo_basico {printf("tipo escalar solo signo \n");}
              | longitud tipo_basico {printf("tipo escalar solo longitud \n");}
              | signo longitud tipo_basico {printf("tipo escalar con signo y longitud \n");}
;

/*************/
/* longitud ::= ’short’ | ’long’ */
/*************/
longitud : SHORT {printf("longitud short \n");}
         | LONG  {printf("longitud long \n");}
;

/*************/
/* signo ::= ’signed’ | ’unsigned’ */
/*************/
signo : SIGNED {printf("signo signed \n");}
         | UNSIGNED  {printf("signo unsigned \n");}
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

dato_indexado : IDENTIFICADOR {printf("dato indexado ID\n");}
               | IDENTIFICADOR '[' expresiones_separadas_por_comas ']' {printf("dato indexado ID con expresiones entre corch \n");}
;

expresiones_separadas_por_comas :
                                 | expresiones_separadas_por_comas ',' expresion {printf("expresiones separadas por comas\n");}
;

/*************/
/* valor ::= expresion| ’[’ ( valor )+ ’]’ */
/*********/

valor : expresion {printf("Valor -> solo expr\n");}
      | '[' valores_separados_por_comas ']' {printf("Valores separados por comas entre corch \n");}
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
declaracion_tipo : nombramiento_tipo {printf("Declaracion tipo -> nomb_tipo\n");}
                 | declaracion_struct_union {printf("Declaracion tipo -> declaracion_struct_union\n");}
                 | declaracion_interfaz {printf("Declaracion tipo -> declaracion_interfaz\n");}
                 | declaracion_enum {printf("Declaracion tipo -> declaracion_enum\n");}
                 | declaracion_clase {printf("Declaracion tipo -> declaracion_clase\n");}
;


/*********/
/* nombramiento_tipo ::= ’typedef’ tipo ID ’;’ */
/*********/
nombramiento_tipo : TYPEDEF tipo IDENTIFICADOR ';'

/*********/
/* declaracion_struct_union ::= [ modificador ]* struct_union [ IDENTIFICADOR ]? ’{’ [ declaracion_campo ]+ ’}’ */
/*********/
declaracion_struct_union : modificadores_opcionales struct_union '{' declaracion_campos '}' {printf("DECLAR STR_UNION SIN ID \n");}
                           | modificadores_opcionales struct_union IDENTIFICADOR '{' declaracion_campos  '}' {printf("DECLAR STR_UNION SIN ID \n");}
;

/*********/
/* MODIFICADORES OPCIONALES */
/*********/
modificadores_opcionales :
                         | modificadores_opcionales modificador {printf("MODS OPCIONALES \n");}
;
/*********/
/* modificador */
/*********/

modificador : NEW {printf("MOD -> NEW\n");}
            | PUBLIC {printf("MOD -> PUBLIC\n");}
            | PROTECTED {printf("MOD -> PUBLIC\n");}
            | INTERNAL {printf("MOD -> INTERNAL\n");}
            | PRIVATE {printf("MOD -> PRIVATE\n");}
            | STATIC {printf("MOD -> STATIC \n");}
            | VIRTUAL {printf("MOD -> VIRTUAL\n");}
            | SEALED {printf("MOD -> SEALED\n");}
            | OVERRIDE {printf("MOD -> OVERRIDE\n");}
            | ABSTRACT {printf("MOD -> ABSTRACT\n");}
            | EXTERN {printf("MOD -> EXTERN\n");}
;
/*********/
/* struct_union ::= ’struct’ | ’union’ */
/*********/
struct_union : STRUCT {printf("struct_union -> STRUCT\n");}
             | UNION  {printf("struct_union -> UNION\n");}
;



/*********/
/* declaracion_campo ::= tipo ( nombre )+ ’;’ | declaracion_struct_union ( nombre )+ ’;’*/
/*********/

/* DEFINICIÓN DE UNO O MÁS DECLARACION_CAMPO */

declaracion_campos : declaracion_campo {printf("UNA SOLA DECL CAMPO \n");}
                    | declaracion_campos declaracion_campo {printf("VARIAS DECL CAMPOS \n");}
;



declaracion_campo : tipo nombres_separados_por_comas ';' {printf("declaracion_campo -> tipo nombre(s)\n");}
                  | declaracion_struct_union nombres_separados_por_comas ';' {printf("declaracion_campo -> declaracion_struct_union nombre(s)\n");}
;

/* DEFINICIÓN DE UNO O MÁS NOMBRES_separados_por_comas */

nombres_separados_por_comas : nombre
                            | nombres_separados_por_comas ',' nombre
;


/*********/
/* declaracion_interfaz ::= [ modificador ]* ’interface’ IDENTIFICADOR [ herencia ]? cuerpo_interfaz */
/*********/

declaracion_interfaz : modificadores_opcionales INTERFACE IDENTIFICADOR cuerpo_interfaz {printf("declaracion_interfaz sin herencia\n");}
                     | modificadores_opcionales INTERFACE IDENTIFICADOR herencia cuerpo_interfaz {printf("declaracion_interfaz con herencia\n");}
;

/*********/
/* herencia ::= ’:’ ( nombre_tipo_o_espacio_nombres )+ */
/*********/
herencia : ':' nombre_tipo_o_espacio_nombres_separados_por_coma {printf("herencia \n");}
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

cuerpo_enum : '{' declaracion_miembro_enum_separados_por_comas '}' {printf("cuerpo_enum\n");}
;

declaracion_miembro_enum_separados_por_comas : declaracion_miembro_enum
                                               | declaracion_miembro_enum_separados_por_comas ',' declaracion_miembro_enum
;

/*********/
/* declaracion_miembro_enum ::= IDENTIFICADOR [ ’=’ expresion ]?’}’ */
/*********/

declaracion_miembro_enum : IDENTIFICADOR {printf("declaracion_miembro_enum sin expresion\n");}
                         | IDENTIFICADOR '=' expresion {printf("declaracion_miembro_enum con expresion\n");}
;

/**********/
/* CLASES */
/**********/

/*********/
/* declaracion_clase ::= [ modificador ]* ’class’ IDENTIFICADOR [ herencia ]? cuerpo_clase */
/*********/

declaracion_clase : modificadores_opcionales CLASS IDENTIFICADOR cuerpo_clase {printf("declaracion_clase sin herencia \n");}
                   | modificadores_opcionales CLASS IDENTIFICADOR herencia cuerpo_clase {printf("declaracion_clase con herencia \n");}
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
;


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

;


/*********/
/* declaracion_metodo ::= [ modificador ]* firma_funcion bloque_instrucciones */
/*********/
declaracion_metodo : modificadores_opcionales firma_funcion bloque_instrucciones {printf("declaracion metodo\n");}
;

/*********/
/* declaracion_constructor ::= [ modificador ]* cabecera_constructor bloque_instrucciones */
/*********/

declaracion_constructor : modificador_constr_opcional cabecera_constructor bloque_instrucciones {printf("DECLARACION CONSTRUCTOR \n");}
;

modificador_constr_opcional :
                            | modificador_constr_opcional modificador_constr
;

modificador_constr :  PUBLIC {printf("MOD CONSTR -> PUBLIC \n");}
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

cabecera_destructor : '~' IDENTIFICADOR {printf("CABECERA DESTR \n");}
                    | '~' IDENTIFICADOR '(' ')' {printf("CABECERA DESTR \n");}
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
                    | MULT_ASIG {printf("OPERADOR ASIG '*=' \n");}
                    | DIV_ASIG {printf("OPERADOR ASIG '/=' \n");}
                    | MOD_ASIG {printf("OPERADOR ASIG MOD ASIG \n");}
                    | SUMA_ASIG {printf("OPERADOR ASIG '+=' \n");}
                    | RESTA_ASIG {printf("OPERADOR ASIG '-=' \n");}
                    | DESPI_ASIG {printf("OPERADOR ASIG '<<=' \n");}
                    | DESPD_ASIG {printf("OPERADOR ASIG '>>=' \n");}
                    | AND_ASIG {printf("OPERADOR ASIG '&=' \n");}
                    | XOR_ASIG {printf("OPERADOR ASIG '^=' \n");}
                    | OR_ASIG {printf("OPERADOR ASIG '|=' \n");}
;

/***************/
/* instruccion_bifurcacion ::= ’if’ ’(’ expresion ’)’ instruccion [ ’else’ instruccion ]? | ’switch’ ’(’ expresion ’)’ ’{’ [ instruccion_caso ]+ ’}’ */
/***************/

instruccion_bifurcacion : IF '(' expresion ')' instruccion {printf("INSTRUCCION BIFUR SOLO IF \n");}
                         | IF '(' expresion ')' instruccion ELSE instruccion {printf("INSTRUCCION BIFUR IF-ELSE \n");}
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
/* instruccion_destino_salto ::= IDENTIFICADOR ’:’ instruccion ’;’ */
/***************/

instruccion_destino_salto : IDENTIFICADOR ':' instruccion ';' {printf("INSTRUCCION SALTO GOTO \n");}

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
clausulas_catch_especificas : clausula_catch_especifica
                            | clausulas_catch_especificas clausula_catch_especifica
;

clausulas_catch_especifica_op :
                              | clausulas_catch_especifica_op clausula_catch_especifica
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
/* expresion_postfija */
/***************/

expresion_postfija : expresion_constante {printf("EXPR POSTFIJA -> CONST \n");}
                    | expresion_parentesis {printf("EXPR POSTFIJA -> PARENTESIS \n");}
                    | expresion_funcional {printf("EXPR POSTFIJA -> FUNCIONAL \n");}
                    | expresion_creacion_objeto {printf("EXPR POSTFIJA -> CREACION_OBJETO \n");}
                    | expresion_indexada {printf("EXPR POSTFIJA -> INDEXADA \n");}
                    | expresion_postfija INC {printf("EXPR POSTFIJA -> INC \n");}
                    | expresion_postfija DEC {printf("EXPR POSTFIJA -> DEC \n");}
;



expresion_parentesis : '(' expresion ')'  {printf("EXPR PARENTESIS \n");}
;
/***************/
/* expresion_funcional ::= identificador_anidado ’(’ ( expresion )* ’) */
/***************/

expresion_funcional : identificador_anidado '(' expresion_op_sep_comas ')'  {printf("EXPR FUNCIONAL \n");}
;

expresion_op_sep_comas :
                        | expresion_op_sep_comas ',' expresion
;


/***************/
/* expresion_creacion_objeto ::= ’new’ identificador_anidado ’(’ ( expresion )* ’)’ */
/***************/

expresion_creacion_objeto : NEW identificador_anidado '(' expresion_op_sep_comas ')'  {printf("EXPR FUNCIONAL \n");}
;


/***************/
/* expresion_indexada ::= identificador_anidado | expresion_indexada ’[’ expresion ’]’ | expresion_indexada ’->’ identificador_anidado */
/***************/

expresion_indexada : identificador_anidado  {printf("EXPR INDEXADA SOLO IDENT ANIDADO \n");}
                    | expresion_indexada '[' expresion ']' {printf("EXPR INDEXADA EXPR \n");}
                    | expresion_indexada PTR_ACCESO identificador_anidado {printf("EXPR INDEXADA PTR_ACCESO \n");}
;


/***************/
/* OPERANDOS*/
/* expresion_constante ::= ENTERO | REAL | CADENA | CARACTER | ’true’ | ’false’ */
/***************/


expresion_constante : ENTERO {printf("EXPR CONST -> ENTERO \n");}
                    | REAL  {printf("EXPR CONST -> REAL \n");}
                    | CADENA  {printf("EXPR CONST -> CADENA \n");}
                    | CARACTER   {printf("EXPR CONST -> CARACTER \n");}
                    | TRUE  {printf("EXPR CONST -> TRUE \n");}
                    | FALSE  {printf("EXPR CONST -> FALSE \n");}
;

/***************/
/* operador_prefijo */
/***************/

operador_prefijo : INC {printf("OP PREFIJO -> INC \n");}
                 | DEC {printf("OP PREFIJO -> DEC \n");}
                 | '&' {printf("OP PREFIJO -> AMPERS \n");}
                 | '*' {printf("OP PREFIJO -> ASTERISCO \n");}
                 | '+' {printf("OP PREFIJO -> MAS \n");}
                 | '-' {printf("OP PREFIJO -> MENOS \n");}
                 | '~' {printf("OP PREFIJO -> VIRGULILLA \n");}
                 | '!' {printf("OP PREFIJO -> EXCLAMACION \n");}
;

/***************/
/* expresion_prefija */
/***************/

expresion_prefija : expresion_postfija
                   | SIZEOF expresion_prefija {printf("EXPR PREFIJA -> SIZEOF \n");}
                   | SIZEOF '(' nombre_tipo ')' {printf("EXPR PREFIJA -> SIZEOF NOMBRE_TIPO \n");}
                   | operador_prefijo expresion_cast {printf("EXPR PREFIJA -> OPERADOR EXPR CAST \n");}
;


/***************/
/* expresion_cast ::= expresion_prefija | ’(’ nombre_tipo ’)’ expresion_prefija */
/***************/

expresion_cast : expresion_prefija
                | '(' nombre_tipo ')' expresion_prefija
;

expresion_logica
    : expresion_logica OR expresion_logica1                                   { printf ("\n\texp_logica -> exp_logica OR exp_logica1\n"); }
    | expresion_logica1                                                       { printf ("\n\texp_logica -> exp_logica1 \n"); }
    ;

expresion_logica1
    : expresion_logica1 AND expresion_logica2                                 { printf ("\n\texp_logica1 -> exp_logica1 AND exp_logica2\n"); }
    | expresion_logica2                                                       { printf ("\n\texp_logica1 -> exp_logica2 \n"); }
    ;

expresion_logica2
    : expresion_logica2 EQ expresion_logica3                                  { printf ("\n\texp_logica2 -> exp_logica2 EQ exp_logica3\n"); }
    | expresion_logica2 NEQ expresion_logica3                                 { printf ("\n\texp_logica2 -> exp_logica2 NEQ exp_logica3\n"); }
    | expresion_logica3
    ;

expresion_logica3
    : expresion_logica3 '<' expresion_logica4                                 { printf ("\n\texp_logica3 -> exp_logica3 < exp_logica4\n"); }
    | expresion_logica3 '>' expresion_logica4                                 { printf ("\n\texp_logica3 -> exp_logica3 > exp_logica4\n"); }
    | expresion_logica3 GE expresion_logica4                                  { printf ("\n\texp_logica3 -> exp_logica3 GE exp_logica4\n"); }
    | expresion_logica3 LE expresion_logica4                                  { printf ("\n\texp_logica3 -> exp_logica3 LE exp_logica4\n"); }
    | expresion_logica4
    ;

expresion_logica4
    : expresion_logica4 '|' expresion_logica5                                 { printf("\n\texp_logica4 -> exp_logica4 | exp_logica5\n"); }
    | expresion_logica5                                                       { printf("\n\texp_logica4 -> exp_logica5 \n"); }
    ;

expresion_logica5
    : expresion_logica5 '^' expresion_logica6                                 { printf ("\n\texp_logica5 -> exp_logica5 '^' exp_logica6\n"); }
    | expresion_logica6                                                       { printf ("\n\texp_logica5 -> exp_logica6 \n"); }
    ;

expresion_logica6
    : expresion_logica6 '&' expresion_logica7                                 { printf ("\n\texp_logica6 -> exp_logica7 & exp_logica6\n"); }
    | expresion_logica7                                                       { printf ("\n\texp_logica6 -> exp_logica7 \n"); }
    ;

expresion_logica7
    : expresion_logica7 DESPI expresion_logica8                               { printf ("\n\texp_logica7 -> exp_logica7 DESPI exp_logica8\n"); }
    | expresion_logica7 DESPD expresion_logica8                               { printf ("\n\texp_logica7 -> exp_logica7 DESPD exp_logica8\n"); }
    | expresion_logica8                                                       { printf ("\n\texp_logica7 -> exp_logica8 \n"); }
    ;

expresion_logica8
    : expresion_logica8 '+' expresion_logica9                                 { printf ("\n\texp_logica8 -> exp_logica8 + exp_logica9\n"); }
    | expresion_logica8 '-' expresion_logica9                                 { printf ("\n\texp_logica8 -> exp_logica8 - exp_logica9\n"); }
    | expresion_logica9                                                       { printf ("\n\texp_logica8 -> exp_logica9\n"); }
    ;

expresion_logica9
    : expresion_logica9 '*' expresion_cast                                    { printf ("\n\texp_logica9 -> exp_logica9 * exp_cast\n"); }
    | expresion_logica9 '/' expresion_cast                                    { printf ("\n\texp_logica9 -> exp_logica9 / exp_cast\n"); }
    | expresion_logica9 '%' expresion_cast                                    { printf ("\n\texp_logica9 -> exp_logica9 % exp_cast\n"); }
    | expresion_cast                                                          { printf ("\n\texp_logica9 -> exp_cast\n"); }
    ;

expresion
    : expresion_logica                                                        { printf ("\n\texp -> exp_logica\n"); }
    | expresion_logica '?' expresion ':' expresion                            { printf ("\n\texp -> exp_logica ? exp : exp\n"); }
    ;

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
    yyin = fopen(argv[1],"r\n");
    yyparse();
    }
  }
