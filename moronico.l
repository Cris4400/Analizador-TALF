%{
#include <stdio.h>
#include "moronico.tab.h"
%}

%option yylineno
%x COM

IDENTIFICADOR [a-zA-Z_][a-zA-Z0-9_]*

ESPACIO [ \t]+

DELIMITADOR ":"|";"|","|"{"|"}"|"("|")"|"|"

OPERADOR_ASIGNACION "="
OPERADORES_ARITMETICOS "+"|"-"|"*"|"/"|"%"
OPERADORES_ACCESO_MEMORIA "."|"["|"]"
OPERADORES_RELACIONALES "<"|">"
OPERADORES_BINARIOS "@"|"~"|"&"

POTENCIA "**"

FLECHA_DOBLE "=>"

LEQ "=<"
GEQ ">="
NEQ "!="
EQ ":="

NEGACION "!"  
AND "\\\/" 
OR "\/\\"

DESPD "->"
DESPI "<-"

CUATRO_PTOS "::"
DOS_PTOS ".."

PATH \"[.]{0,2}[/][^\n\"]*\"\"

CTC_ENTERA [0-9]+|\\[0-7]+|\\(?i:x)[0-9a-fA-F]+
CTC_REAL [0-9]*['][0-9]+|[0-9]*[']?[0-9]+[\^][+-]?[0-9]+
CTC_BOOLEANA "verdadero"|"falso"
CTC_CARACTER \'[^\n\'\"\?\\]\'|\'\\[\'\"\?\\abfnrtv]\'|\'25[0-5]\'|\'2[0-4][0-9]\'|\'[01]?[0-9][0-9]\'|\'\\[0-3]?[0-7]?[0-7]\'|\'\\x[0-9a-fA-F]{1,2}\'
CTC_CADENA (\")([^\n\'\"\?\\]|[\\][\'\"\?\\abfnrtv]|(\\([0-9]|[1-9][0-9]|1[0-9]{2}|2[0-4][0-9]|25[0-4]))|(\\([0-7]{1,2}|[0-3][0-7]{2}))|(\\x([0-9a-fA-F]{1,2}))|\\\n)*(\")

COMENTARIO "//".*

%%
[ \t\n]+ { }

(?i:abstracto) { printf("\nlinea %d, palabra reservada: %s", yylineno, yytext); return ABSTRACTO;}
(?i:asociativa) { printf("\nlinea %d, palabra reservada: %s", yylineno, yytext); return ASOCIATIVA;}
(?i:booleano) { printf("\nlinea %d, palabra reservada: %s", yylineno, yytext); return BOOLEANO;}
(?i:cabecera) { printf("\nlinea %d, palabra reservada: %s", yylineno, yytext); return CABECERA;}
(?i:cadena) { printf("\nlinea %d, palabra reservada: %s", yylineno, yytext); return CADENA;}
(?i:caso) { printf("\nlinea %d, palabra reservada: %s", yylineno, yytext); return CASO;}
(?i:caracter) { printf("\nlinea %d, palabra reservada: %s", yylineno, yytext); return CARACTER;}
(?i:clase) { printf("\nlinea %d, palabra reservada: %s", yylineno, yytext); return CLASE;}
(?i:conjunto) { printf("\nlinea %d, palabra reservada: %s", yylineno, yytext); return CONJUNTO;}
(?i:constante) { printf("\nlinea %d, palabra reservada: %s", yylineno, yytext); return CONSTANTE;}
(?i:cuerpo) { printf("\nlinea %d, palabra reservada: %s", yylineno, yytext); return CUERPO;}

(?i:constructor) { printf("\nlinea %d, palabra reservada: %s", yylineno, yytext); return CONSTRUCTOR;}
(?i:cuando) { printf("\nlinea %d, palabra reservada: %s", yylineno, yytext); return CUANDO;}
(?i:descendente) { printf("\nlinea %d, palabra reservada: %s", yylineno, yytext); return DESCENDENTE;}
(?i:destructor) { printf("\nlinea %d, palabra reservada: %s", yylineno, yytext); return DESTRUCTOR;}
(?i:de) { printf("\nlinea %d, palabra reservada: %s", yylineno, yytext); return DE;}
(?i:devolver) { printf("\nlinea %d, palabra reservada: %s", yylineno, yytext); return DEVOLVER;}
(?i:carga) { printf("\nlinea %d, palabra reservada: %s", yylineno, yytext); return CARGA;}
(?i:elemento) { printf("\nlinea %d, palabra reservada: %s", yylineno, yytext); return ELEMENTO;}
(?i:ejecuta) { printf("\nlinea %d, palabra reservada: %s", yylineno, yytext); return EJECUTA;}
(?i:en) { printf("\nlinea %d, palabra reservada: %s", yylineno, yytext); return EN;}
(?i:entero) { printf("\nlinea %d, palabra reservada: %s", yylineno, yytext); return ENTERO;}

(?i:entonces) { printf("\nlinea %d, palabra reservada: %s", yylineno, yytext); return ENTONCES;}
(?i:especifico) { printf("\nlinea %d, palabra reservada: %s", yylineno, yytext); return ESPECIFICO;}
(?i:excepto) { printf("\nlinea %d, palabra reservada: %s", yylineno, yytext); return EXCEPTO;}
(?i:fichero) { printf("\nlinea %d, palabra reservada: %s", yylineno, yytext); return FICHERO;}
(?i:final) { printf("\nlinea %d, palabra reservada: %s", yylineno, yytext); return FINAL;}
(?i:finalmente) { printf("\nlinea %d, palabra reservada: %s", yylineno, yytext); return FINALMENTE;}
(?i:funcion) { printf("\nlinea %d, palabra reservada: %s", yylineno, yytext); return FUNCION;}
(?i:generico) { printf("\nlinea %d, palabra reservada: %s", yylineno, yytext); return GENERICO;}
(?i:hasta) { printf("\nlinea %d, palabra reservada: %s", yylineno, yytext); return HASTA;}
(?i:interfaz) { printf("\nlinea %d, palabra reservada: %s", yylineno, yytext); return INTERFAZ;}

(?i:lanzar) { printf("\nlinea %d, palabra reservada: %s", yylineno, yytext); return LANZAR;}
(?i:lista) { printf("\nlinea %d, palabra reservada: %s", yylineno, yytext); return LISTA;}
(?i:mientras) { printf("\nlinea %d, palabra reservada: %s", yylineno, yytext); return MIENTRAS;}
(?i:modificable) { printf("\nlinea %d, palabra reservada: %s", yylineno, yytext); return MODIFICABLE;}
(?i:otro) { printf("\nlinea %d, palabra reservada: %s", yylineno, yytext); return OTRO;}
(?i:paquete) { printf("\nlinea %d, palabra reservada: %s", yylineno, yytext); return PAQUETE;}
(?i:para) { printf("\nlinea %d, palabra reservada: %s", yylineno, yytext); return PARA;}
(?i:privado) { printf("\nlinea %d, palabra reservada: %s", yylineno, yytext); return PRIVADO;}
(?i:probar) { printf("\nlinea %d, palabra reservada: %s", yylineno, yytext); return PROBAR;}
(?i:procedimiento) { printf("\nlinea %d, palabra reservada: %s", yylineno, yytext); return PROCEDIMIENTO;}
(?i:programa) { printf("\nlinea %d, palabra reservada: %s", yylineno, yytext); return PROGRAMA;}

(?i:publico) { printf("\nlinea %d, palabra reservada: %s", yylineno, yytext); return PUBLICO;}
(?i:real) { printf("\nlinea %d, palabra reservada: %s", yylineno, yytext); return REAL;}
(?i:registro) { printf("\nlinea %d, palabra reservada: %s", yylineno, yytext); return REGISTRO;}
(?i:repite) { printf("\nlinea %d, palabra reservada: %s", yylineno, yytext); return REPITE;}
(?i:salir) { printf("\nlinea %d, palabra reservada: %s", yylineno, yytext); return SALIR;}
(?i:sea) { printf("\nlinea %d, palabra reservada: %s", yylineno, yytext); return SEA;}
(?i:semipublico) { printf("\nlinea %d, palabra reservada: %s", yylineno, yytext); return SEMIPUBLICO;}
(?i:si) { printf("\nlinea %d, palabra reservada: %s", yylineno, yytext); return SI;}
(?i:sino) { printf("\nlinea %d, palabra reservada: %s", yylineno, yytext); return SINO;}
(?i:tipo) { printf("\nlinea %d, palabra reservada: %s", yylineno, yytext); return TIPO;}
(?i:variable) { printf("\nlinea %d, palabra reservada: %s", yylineno, yytext); return VARIABLE;}

{IDENTIFICADOR} { printf("\nlinea %d, identificador: %s", yylineno, yytext); return IDENTIFICADOR;}

{CTC_ENTERA} { printf("\nlinea %d, constante entera: %s", yylineno, yytext); return CTC_ENTERA;}
{CTC_REAL} { printf("\nlinea %d, constante real: %s", yylineno, yytext); return CTC_REAL;}
{CTC_CARACTER} { printf("\nlinea %d, constante caracter: %s", yylineno, yytext); return CTC_CARACTER;}

{PATH} { printf("\nlinea %d, constante path: %s", yylineno, yytext); return PATH;}
{CTC_CADENA} { printf("\nlinea %d, constante cadena: %s", yylineno, yytext); return CTC_CADENA;}

{DELIMITADOR} { printf("\nlinea %d, delimitador: %s", yylineno, yytext); return yytext[0];}

{FLECHA_DOBLE} { printf("\nlinea %d, delimitador: %s", yylineno, yytext); return FLECHA_DOBLE;}
{CUATRO_PTOS} { printf("\nlinea %d, delimitador: %s", yylineno, yytext); return CUATRO_PTOS;}
{DOS_PTOS} { printf("\nlinea %d, delimitador: %s", yylineno, yytext); return DOS_PTOS;}

{OPERADOR_ASIGNACION} { printf("\nlinea %d, operador: %s", yylineno, yytext); return yytext[0];}

{OPERADORES_ARITMETICOS} { printf("\nlinea %d, operador: %s", yylineno, yytext); return yytext[0];}
{POTENCIA} { printf("\nlinea %d, operador: %s", yylineno, yytext); return POTENCIA;}

{OPERADORES_ACCESO_MEMORIA} { printf("\nlinea %d, operador: %s", yylineno, yytext); return yytext[0];}

{OPERADORES_RELACIONALES} { printf("\nlinea %d, operador: %s", yylineno, yytext); return yytext[0];}
{LEQ} { printf("\nlinea %d, operador: %s", yylineno, yytext); return LEQ;}
{GEQ} { printf("\nlinea %d, operador: %s", yylineno, yytext); return GEQ;}
{EQ} { printf("\nlinea %d, operador: %s", yylineno, yytext); return EQ;}
{NEQ} { printf("\nlinea %d, operador: %s", yylineno, yytext); return NEQ;}

{OPERADORES_BINARIOS} { printf("\nlinea %d, operador: %s", yylineno, yytext); return yytext[0];}
{DESPD} { printf("\nlinea %d, operador: %s", yylineno, yytext); return DESPD;}
{DESPI} { printf("\nlinea %d, operador: %s", yylineno, yytext); return DESPI;}

{NEGACION} { printf("\nlinea %d, operador: %s", yylineno, yytext); return yytext[0];}
{AND} { printf("\nlinea %d, operador: %s", yylineno, yytext); return AND;}
{OR} { printf("\nlinea %d, operador: %s", yylineno, yytext); return OR;}

{COMENTARIO} { }

{ESPACIO}*"/*"                          BEGIN COM;
<COM>.|\n                          ;
<COM>"*/"({ESPACIO}*)*                BEGIN 0;

. { printf("\nlinea %d, error: %s", yylineno, yytext); }

%%