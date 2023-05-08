%{
  #include <stdio.h>
  extern FILE *yyin;
  extern int yylex();

  void yyerror(char *s);

  #define YYDEBUG 1

%}

%token ABSTRACTO AND ASOCIATIVA BOOLEANO 
%token CABECERA CADENA CASO CARACTER 
%token CARGA CLASE CONJUNTO CONSTANTE 
%token CUERPO CTC_BOOLEANA CTC_CADENA CTC_CARACTER 
%token CTC_ENTERA CTC_REAL CONSTRUCTOR CUANDO
%token CUATRO_PTOS DESCENDENTE DESPD DESPI 
%token DESTRUCTOR DE DEVOLVER DOS_PTOS 
%token EJECUTA ELEMENTO EN ENTERO 
%token ENTONCES EQ ESPECIFICO EXCEPTO 
%token FICHERO FINAL FINALMENTE FLECHA_DOBLE 
%token FUNCION GEQ GENERICO HASTA 
%token IDENTIFICADOR INTERFAZ LANZAR LEQ 
%token LISTA MIENTRAS MODIFICABLE NEQ 
%token OTRO OR PAQUETE PARA 
%token PATH POTENCIA PRIVADO PROBAR 
%token PROCEDIMIENTO PROGRAMA PUBLICO REAL 
%token REGISTRO REPITE SALIR SEA 
%token SEMIPUBLICO SI SINO TIPO VARIABLE

/*%right OR
%right AND
%nonassoc '!'
%left '<' '>' LEQ GEQ EQ NEQ
%left '@'
%left '~'
%left '&'
%left DESPI DESPD
%left '+' '-'
%left '*' '/' '%'
%right POTENCIA
%nonassoc UNARIO*/

%start programa

%%

/*--------------------PROGRAMAS, PAQUETES, CARGAS----------------------*/

programa: definicion_programa { printf("\nEXITO: programa -> def_prog\n"); }
        | definicion_paquete  { printf("\nEXITO: programa -> def_paq\n"); }       
;

definicion_programa: PROGRAMA nombre ';' bloque_programa { printf("\n  def_prog -> PROGRAMA nombre_decl ; blq_prog"); };

nombre: IDENTIFICADOR { printf("\n  nombre_decl -> IDENTIFICADOR"); }
      | nombre CUATRO_PTOS IDENTIFICADOR { printf("\n  nombre_decl -> nombre_decl :: IDENTIFICADOR"); }
;  

/* Uno o más nombres separados por comas*/
nombres_mas: nombre { printf("\n  nombres_decls -> nombre_decl"); }
           | nombres_mas ',' nombre { printf("\n  nombres_decls -> nombres_decls , nombre_decl"); }
;           

bloque_programa: bloque_instrucciones {printf("\n  blq_prog -> blq_instrs"); } 

               | declaracion_cargas bloque_instrucciones {printf("\n  blq_prog -> decl_cargas blq_instrs"); }
               | declaracion_tipos bloque_instrucciones {printf("\n  blq_prog -> decl_tipos blq_instrs"); }
               | declaracion_constantes bloque_instrucciones {printf("\n  blq_prog -> decl_consts blq_instrs"); }
               | declaracion_variables bloque_instrucciones {printf("\n  blq_prog -> decl_vars blq_instrs"); }

               | declaracion_cargas declaracion_tipos bloque_instrucciones {printf("\n  blq_prog -> decl_cargas decl_tipos blq_instrs"); }
               | declaracion_cargas declaracion_constantes bloque_instrucciones {printf("\n  blq_prog -> decl_cargas decl_consts blq_instrs"); }
               | declaracion_cargas declaracion_variables bloque_instrucciones {printf("\n  blq_prog -> decl_cargas decl_vars blq_instrs"); }
               | declaracion_tipos declaracion_constantes bloque_instrucciones {printf("\n  blq_prog -> decl_tipos decl_consts blq_instrs"); }
               | declaracion_tipos declaracion_variables bloque_instrucciones {printf("\n  blq_prog -> decl_tipos decl_vars blq_instrs"); }
               | declaracion_constantes declaracion_variables bloque_instrucciones {printf("\n  blq_prog -> decl_consts decl_vars blq_instrs"); }

               | declaracion_cargas declaracion_tipos declaracion_constantes bloque_instrucciones {printf("\n  blq_prog -> decl_cargas decl_tipos decl_consts blq_instrs"); }
               | declaracion_cargas declaracion_tipos declaracion_variables bloque_instrucciones {printf("\n  blq_prog -> decl_cargas decl_tipos decl_vars blq_instrs"); }
               | declaracion_cargas declaracion_constantes declaracion_variables bloque_instrucciones {printf("\n  blq_prog -> decl_cargas decl_consts decl_vars blq_instrs"); }
               | declaracion_tipos declaracion_constantes declaracion_variables bloque_instrucciones {printf("\n  blq_prog -> decl_tipos decl_consts decl_vars blq_instrs"); }

               | declaracion_cargas declaracion_tipos declaracion_constantes declaracion_variables bloque_instrucciones {printf("\n  blq_prog -> decl_cargas decl_tipos decl_consts decl_vars blq_instrs"); }
;                         

bloque_instrucciones: '{' instrucciones '}' { printf("\n  blq_instrs -> '{' instrs '}'"); } ;

definicion_paquete: PAQUETE nombre ';' seccion_cabecera seccion_cuerpo { printf("\n  def_paq -> PAQUETE nombre_decl ; seccion_cab seccion_cuerpo"); } ;

seccion_cabecera: CABECERA { printf("\n  seccion_cab -> CABECERA"); }

                | CABECERA declaracion_cargas { printf("\n  seccion_cab -> CABECERA decl_cargas"); }
                | CABECERA declaracion_tipos { printf("\n  seccion_cab -> CABECERA decl_tipos"); }
                | CABECERA declaracion_constantes { printf("\n  seccion_cab -> CABECERA decl_consts"); }
                | CABECERA declaracion_variables { printf("\n  seccion_cab -> CABECERA decl_vars"); }
                | CABECERA declaracion_interfaces { printf("\n  seccion_cab -> CABECERA decl_intf"); }

                | CABECERA declaracion_cargas declaracion_tipos {printf("\n  seccion_cab -> CABECERA decl_cargas decl_tipos"); }
                | CABECERA declaracion_cargas declaracion_constantes {printf("\n  seccion_cab -> CABECERA decl_cargas decl_consts"); }
                | CABECERA declaracion_cargas declaracion_variables {printf("\n  seccion_cab -> CABECERA decl_cargas decl_vars"); }
                | CABECERA declaracion_cargas declaracion_interfaces {printf("\n  seccion_cab -> CABECERA decl_cargas decl_intf"); }
                | CABECERA declaracion_tipos declaracion_constantes {printf("\n  seccion_cab -> CABECERA decl_tipos decl_consts"); }
                | CABECERA declaracion_tipos declaracion_variables {printf("\n  seccion_cab -> CABECERA decl_tipos decl_vars"); }
                | CABECERA declaracion_tipos declaracion_interfaces {printf("\n  seccion_cab -> CABECERA decl_tipos decl_intf"); }
                | CABECERA declaracion_constantes declaracion_variables {printf("\n  seccion_cab -> CABECERA decl_consts decl_vars"); }
                | CABECERA declaracion_constantes declaracion_interfaces {printf("\n  seccion_cab -> CABECERA decl_consts decl_intf"); }
                | CABECERA declaracion_variables declaracion_interfaces {printf("\n  seccion_cab -> CABECERA decl_vars decl_intf"); }

                | CABECERA declaracion_cargas declaracion_tipos declaracion_constantes {printf("\n  seccion_cab -> CABECERA decl_cargas decl_tipos decl_consts"); }
                | CABECERA declaracion_cargas declaracion_tipos declaracion_variables {printf("\n  seccion_cab -> CABECERA decl_cargas decl_tipos decl_vars"); }
                | CABECERA declaracion_cargas declaracion_tipos declaracion_interfaces {printf("\n  seccion_cab -> CABECERA decl_cargas decl_tipos decl_intf"); }
                | CABECERA declaracion_cargas declaracion_constantes declaracion_variables {printf("\n  seccion_cab -> CABECERA decl_cargas decl_consts decl_vars"); }
                | CABECERA declaracion_cargas declaracion_constantes declaracion_interfaces {printf("\n  seccion_cab -> CABECERA decl_cargas decl_consts decl_intf"); }
                | CABECERA declaracion_cargas declaracion_variables declaracion_interfaces {printf("\n  seccion_cab -> CABECERA decl_cargas decl_vars decl_intf"); }
                | CABECERA declaracion_tipos declaracion_constantes declaracion_variables {printf("\n  seccion_cab -> CABECERA decl_tipos decl_consts decl_vars"); }
                | CABECERA declaracion_tipos declaracion_constantes declaracion_interfaces {printf("\n  seccion_cab -> CABECERA decl_tipos decl_consts decl_intf"); }
                | CABECERA declaracion_tipos declaracion_variables declaracion_interfaces {printf("\n  seccion_cab -> CABECERA  decl_tipos decl_vars decl_intf"); }
                | CABECERA declaracion_constantes declaracion_variables declaracion_interfaces {printf("\n  seccion_cab -> CABECERA decl_consts decl_vars decl_intf"); }

                | CABECERA declaracion_cargas declaracion_tipos declaracion_constantes declaracion_variables {printf("\n  seccion_cab -> CABECERA decl_cargas decl_tipos decl_consts decl_vars"); }
                | CABECERA declaracion_cargas declaracion_tipos declaracion_constantes declaracion_interfaces {printf("\n  seccion_cab -> CABECERA decl_cargas decl_tipos decl_consts decl_intf"); }
                | CABECERA declaracion_cargas declaracion_tipos declaracion_variables declaracion_interfaces {printf("\n  seccion_cab -> CABECERA decl_cargas decl_tipos decl_vars decl_intf"); }
                | CABECERA declaracion_cargas declaracion_constantes declaracion_variables declaracion_interfaces {printf("\n  seccion_cab -> CABECERA decl_cargas decl_consts decl_vars decl_intf"); }
                | CABECERA declaracion_tipos declaracion_constantes declaracion_variables declaracion_interfaces {printf("\n  seccion_cab -> CABECERA decl_tipos decl_consts decl_vars decl_intf"); }

                | CABECERA declaracion_cargas declaracion_tipos declaracion_constantes declaracion_variables declaracion_interfaces {printf("\n  seccion_cab -> CABECERA decl_cargas decl_tipos decl_consts decl_vars decl_intf"); }
;

seccion_cuerpo: CUERPO declaraciones_subprogramas_mas { printf("\n  seccion_cuerpo -> CUERPO decls_subprgs"); }

              | CUERPO declaracion_tipos declaraciones_subprogramas_mas { printf("\n  seccion_cuerpo -> CUERPO decl_tipos decls_subprgs"); }
              | CUERPO declaracion_constantes declaraciones_subprogramas_mas { printf("\n  seccion_cuerpo -> CUERPO decl_consts decls_subprgs"); }
              | CUERPO declaracion_variables declaraciones_subprogramas_mas { printf("\n  seccion_cuerpo -> CUERPO decl_vars decls_subprgs"); }
              
              | CUERPO declaracion_tipos declaracion_constantes declaraciones_subprogramas_mas { printf("\n  seccion_cuerpo -> CUERPO decl_tipos decl_consts decls_subprgs"); }
              | CUERPO declaracion_tipos declaracion_variables declaraciones_subprogramas_mas { printf("\n  seccion_cuerpo -> CUERPO decl_tipos decl_vars decls_subprgs"); }
              | CUERPO declaracion_constantes declaracion_variables declaraciones_subprogramas_mas { printf("\n  seccion_cuerpo -> CUERPO decl_consts decl_vars decls_subprgs"); }
              
              | CUERPO declaracion_tipos declaracion_constantes declaracion_variables declaraciones_subprogramas_mas { printf("\n  seccion_cuerpo -> CUERPO decl_tipos decl_consts decl_vars decls_subprgs"); }
;

declaracion_cargas: CARGA declaraciones_cargas_mas ';' { printf("\n  decl_cargas -> CARGA decls_cargas"); } ;         

declaracion_carga: nombre { printf("\n  decl_carga -> nombre_decl"); }
                 | nombre EN PATH { printf("\n  decl_carga -> nombre_decl EN PATH"); }
                 | nombre '(' nombres_mas ')' { printf("\n  decl_carga -> nombre_decl ( nombres_decls )"); }
                 | nombre EN PATH '(' nombres_mas ')' { printf("\n  decl_carga -> nombre_decl EN PATH ( nombres_decls )"); }
;

declaraciones_cargas_mas: declaracion_carga { printf("\n  decls_cargas -> decl_carga"); } 
                        | declaraciones_cargas_mas ',' declaracion_carga { printf("\n  decls_cargas -> decls_cargas , decl_carga"); } 
;                        

/*---------------------------------------------------------------------*/

/*------------------------TIPOS (INCL. CLASES)-------------------------*/

declaracion_tipos: TIPO declaraciones_tipos { printf("\n  decl_tipos -> TIPO decls_tipos "); } ;

declaracion_tipo: nombre '=' tipo_no_estructurado_o_nombre_tipo ';' { printf("\n  decl_tipo -> nombre_decl = tipo_no_str_o_nom ;"); }
                | nombre '=' tipo_estructurado { printf("\n  decl_tipo -> nombre_decl = tipo_str"); }
;

/* Uno o más declaraciones de tipos */
declaraciones_tipos: declaracion_tipo { printf("\n  decls_tipos -> decl_tipo "); } ;
                   | declaraciones_tipos declaracion_tipo { printf("\n  decls_tipos -> decls_tipos decl_tipo "); } ;
;         

tipo_no_estructurado_o_nombre_tipo: nombre { printf("\n  tipo_no_str_o_nom -> nombre_decl"); }
                                  | tipo_escalar { printf("\n  tipo_no_str_o_nom -> tipo_escalar"); }
                                  | tipo_fichero { printf("\n  tipo_no_str_o_nom -> tipo_fichero"); }
                                  | tipo_enumerado { printf("\n  tipo_no_str_o_nom -> tipo_enumerado"); }
                                  | tipo_lista { printf("\n  tipo_no_str_o_nom -> tipo_lista"); }
                                  | tipo_lista_asociativa { printf("\n  tipo_no_str_o_nom -> tipo_lista_asociativa"); }
                                  | tipo_conjunto { printf("\n  tipo_no_str_o_nom -> tipo_conjunto"); }
;  

tipo_estructurado: tipo_registro { printf("\n  tipo_str -> tipo_registro"); }
                 | declaracion_clase { printf("\n  tipo_str -> decl_clase"); }
;  

tipo_escalar: ENTERO { printf("\n  tipo_escalar -> ENTERO"); } 
            | REAL { printf("\n  tipo_escalar -> REAL"); } 
            | BOOLEANO { printf("\n  tipo_escalar -> BOOLEANO"); } 
            | CARACTER { printf("\n  tipo_escalar -> CARACTER"); }  
            | CADENA { printf("\n  tipo_escalar -> CADENA"); } 
;  

tipo_fichero: FICHERO { printf("\n  tipo_fichero -> FICHERO");} ;

tipo_enumerado: '(' expresiones_constantes_mas ')' { printf("\n  tipo_enumerado -> exprs_consts");} ;

tipo_lista: LISTA DE tipo_no_estructurado_o_nombre_tipo { printf("\n  tipo_lista -> LISTA DE tipo_no_str_o_nom"); }
          | LISTA '[' rangos_mas ']' DE tipo_no_estructurado_o_nombre_tipo { printf("\n  tipo_lista -> LISTA [ rangos ] DE tipo_no_str_o_nom"); }
;      

tipo_lista_asociativa: LISTA ASOCIATIVA DE tipo_no_estructurado_o_nombre_tipo { printf("\n  tipo_lista_asociativa -> LISTA ASOCIATIVA DE tipo_no_str_o_nom"); } ;

tipo_conjunto: CONJUNTO DE tipo_no_estructurado_o_nombre_tipo { printf("\n  tipo_conjunto -> CONJUNTO DE tipo_no_str_o_nom"); };

rango: expresion DOS_PTOS expresion { printf("\n  rango -> expr .. expr"); }
     | expresion DOS_PTOS expresion DOS_PTOS expresion { printf("\n  rango -> expr .. expr .. expr"); }
;  

rangos_mas: rango { printf("\n  rangos -> rango"); }
          | rangos_mas ',' rango { printf("\n  rangos -> rangos , rango"); }
;          

tipo_registro: REGISTRO '{' declaraciones_campos_mas '}' { printf("\n  tipo_registro -> REGISTRO { decls_campos }"); };

declaracion_campo: nombres_mas ':' tipo_no_estructurado_o_nombre_tipo ';' { printf("\n  decl_campo -> nombres_decl : tipo_no_str_o_nom"); };

declaraciones_campos_mas: declaracion_campo { printf("\n  decls_campos -> decl_campo"); };
                        | declaraciones_campos_mas declaracion_campo { printf("\n  decls_campos -> decls_campos decl_campo"); };
;   

declaracion_clase: CLASE '{' declaraciones_publicas '}' { printf("\n  decl_clase -> CLASE { decl_public }"); }
                 | CLASE '{' declaraciones_publicas declaraciones_semi'}' { printf("\n  decl_clase -> CLASE { decl_public decl_semi}"); }
                 | CLASE '{' declaraciones_publicas declaraciones_privadas'}' { printf("\n  decl_clase -> CLASE { decl_public decl_priv }"); }
                 | CLASE '{' declaraciones_publicas declaraciones_semi declaraciones_privadas'}' { printf("\n  decl_clase -> CLASE { decl_public decl_semi decl_priv}"); }

                 | CLASE FINAL '{' declaraciones_publicas '}' { printf("\n  decl_clase -> CLASE FINAL { decl_public }"); }
                 | CLASE FINAL '{' declaraciones_publicas declaraciones_semi'}' { printf("\n  decl_clase -> CLASE FINAL { decl_public decl_semi}"); }
                 | CLASE FINAL '{' declaraciones_publicas declaraciones_privadas'}' { printf("\n  decl_clase -> CLASE FINAL { decl_public decl_priv }"); }
                 | CLASE FINAL '{' declaraciones_publicas declaraciones_semi declaraciones_privadas'}' { printf("\n  decl_clase -> CLASE FINAL { decl_public decl_semi decl_priv}"); }

                 | CLASE '(' nombres_mas ')' '{' declaraciones_publicas '}' { printf("\n  decl_clase -> CLASE ( nombres_decls ) { decl_public }"); }
                 | CLASE '(' nombres_mas ')' '{' declaraciones_publicas declaraciones_semi'}' { printf("\n  decl_clase -> CLASE ( nombres_decls ) { decl_public decl_semi}"); }
                 | CLASE '(' nombres_mas ')' '{' declaraciones_publicas declaraciones_privadas'}' { printf("\n  decl_clase -> CLASE ( nombres_decls ) { decl_public decl_priv }"); }
                 | CLASE '(' nombres_mas ')' '{' declaraciones_publicas declaraciones_semi declaraciones_privadas'}' { printf("\n  decl_clase -> CLASE ( nombres_decls ) { decl_public decl_semi decl_priv}"); }

                 | CLASE FINAL '(' nombres_mas ')' '{' declaraciones_publicas '}' { printf("\n  decl_clase -> CLASE FINAL ( nombres_decls ) { decl_public }"); }
                 | CLASE FINAL '(' nombres_mas ')' '{' declaraciones_publicas declaraciones_semi'}' { printf("\n  decl_clase -> CLASE FINAL ( nombres_decls ) { decl_public decl_semi}"); }
                 | CLASE FINAL '(' nombres_mas ')' '{' declaraciones_publicas declaraciones_privadas'}' { printf("\n  decl_clase -> CLASE FINAL ( nombres_decls ) { decl_public decl_priv }"); }
                 | CLASE FINAL '(' nombres_mas ')' '{' declaraciones_publicas declaraciones_semi declaraciones_privadas'}' { printf("\n  decl_clase -> CLASE FINAL ( nombres_decls ) { decl_public decl_semi decl_priv}"); }
;

declaraciones_publicas: declaraciones_componentes_mas { printf("\n  decl_public -> decls_comps"); }
                      | PUBLICO declaraciones_componentes_mas { printf("\n  decl_public -> PUBLICO decls_comps"); }
;

declaraciones_semi: SEMIPUBLICO declaraciones_componentes_mas { printf("\n  decl_semi -> SEMIPUBLICO decls_comps"); };

declaraciones_privadas: PRIVADO declaraciones_componentes_mas { printf("\n  decl_priv -> PRIVADO decls_comps"); };;

declaracion_componente: declaracion_tipo_anidado { printf("\n  decl_comp -> decl_tipo_anidado"); }
                      | declaracion_constante_anidada { printf("\n  decl_comp -> decl_const_anidada"); }
                      | declaracion_atributos { printf("\n  decl_comp -> decl_atributos"); }
                      | cabecera_subprograma ';' { printf("\n  decl_comp -> cab_subprg ;"); }
                      | cabecera_subprograma ';' modificadores_mas ';' { printf("\n  decl_comp -> cab_subprg ; modifics ;"); }
; 

declaraciones_componentes_mas: declaracion_componente { printf("\n  decls_comps -> decl_comp"); }
                             | declaraciones_componentes_mas declaracion_componente { printf("\n  decls_comps -> decls_comps decl_comp"); }
;                             

declaracion_tipo_anidado: TIPO declaracion_tipo { printf("\n  decl_tipo_anidado -> TIPO decl_tipo"); };

declaracion_constante_anidada: CONSTANTE declaracion_constante { printf("\n  decl_const_anidada -> CONSTANTE decl_const"); };

declaracion_atributos: nombres_mas ':' tipo_no_estructurado_o_nombre_tipo ';' { printf("\n  decl_atributos -> nombres_decls : tipo_no_str_o_nom ;"); };

modificadores_mas: modificador { printf("\n  modifics -> modific"); }
                 | modificadores_mas ',' modificador { printf("\n  modifics -> modifics , modific"); }
;                 

modificador: GENERICO { printf("\n  modific -> GENERICO"); }
           | ABSTRACTO { printf("\n  modific -> ABSTRACTO"); }
           | ESPECIFICO { printf("\n  modific -> ESPECIFICO"); }
           | FINAL { printf("\n  modific -> FINAL"); }
;  

/*---------------------------------------------------------------------*/


/*------------------CONSTANTES, VARIABLES, INTERFACES------------------*/

declaracion_constantes: CONSTANTE declaraciones_constantes_mas { printf("\n  decl_consts -> CONSTANTE decls_consts"); } ;

declaracion_constante: nombre ':' tipo_no_estructurado_o_nombre_tipo '=' valor_constante ';' { printf("\n  decl_const -> nombre_decl : tipo_no_str_o_nom = valor_const ;"); } ; 

declaraciones_constantes_mas: declaracion_constante { printf("\n  decls_consts -> decl_const"); }
                            | declaraciones_constantes_mas declaracion_constante { printf("\n  decls_consts -> decls_consts decl_const"); }
;      

valor_constante: expresion { printf("\n  valor_const -> expr"); }
              | '[' valores_constantes_mas ']' { printf("\n  valor_const -> '[' valors_consts ']'"); }
              | '[' claves_valores_mas ']' { printf("\n  valor_const -> '[' claves_valores ']'"); }
              | '[' campos_valores_mas ']' { printf("\n  valor_const -> '[' campos_valores ']'"); }
;    

valores_constantes_mas: valor_constante { printf("\n  valors_consts -> valor_const"); }
                      | valores_constantes_mas ',' valor_constante { printf("\n  valors_consts -> valors_consts , valor_const"); }
;                      

clave_valor: CTC_CADENA FLECHA_DOBLE valor_constante { printf("\n  clave_valor -> CTC_CADENA => valor_const"); } ;

claves_valores_mas: clave_valor { printf("\n  claves_valores -> clave_valor"); }
                  | claves_valores_mas ',' clave_valor { printf("\n  claves_valores -> claves_valores , clave_valor"); }
;   

campo_valor: nombre FLECHA_DOBLE valor_constante { printf("\n  campo_valor -> nombre_decl => valor_const"); } ;

campos_valores_mas: campo_valor { printf("\n  campos_valores -> campo_valor"); }
                  | campos_valores_mas ',' campo_valor { printf("\n  campos_valores -> campos_valores , campo_valor"); }
;     

declaracion_variables: VARIABLE declaraciones_variables_mas { printf("\n  decl_var -> VARIABLE decls_vars"); } ;

declaracion_variable: nombres_mas ':' tipo_no_estructurado_o_nombre_tipo ';' { printf("\n  decl_var -> nombres_decls : tipo_no_str_o_nom ;"); }
                    | nombres_mas ':' tipo_no_estructurado_o_nombre_tipo '=' valor_constante ';' { printf("\n  decl_var -> nombres_decls : tipo_no_str_o_nom = valor_const;"); }
;

declaraciones_variables_mas: declaracion_variable { printf("\n  decls_vars -> decl_var"); }
                           | declaraciones_variables_mas declaracion_variable { printf("\n  decls_vars -> decls_vars decl_var"); }
;

declaracion_interfaces: INTERFAZ cabeceras_subprogramas_mas ';' { printf("\n  decl_intf -> INTERFAZ cabs_subprgs"); };

/*---------------------------------------------------------------------*/


/*---------------------------SUBPROGRAMAS-----------------------------*/

declaracion_subprograma: cabecera_subprograma bloque_subprograma { printf("\n  decl_subprg -> cab_subprg blq_subprg"); } ;

declaraciones_subprogramas_mas: declaracion_subprograma { printf("\n  decls_subprgs -> decl_subprg"); }
                              | declaraciones_subprogramas_mas declaracion_subprograma { printf("\n  decls_subprgs -> decls_subprgs decl_subprg"); }
;                              

cabecera_subprograma: cabecera_funcion { printf("\n  cab_subprg -> cab_func"); }
                    | cabecera_procedimiento { printf("\n  cab_subprg -> cab_proced"); }
                    | cabecera_constructor { printf("\n  cab_subprg -> cab_constr"); }
                    | cabecera_destructor { printf("\n  cab_subprg -> cab_destr"); }
;

cabeceras_subprogramas_mas: cabecera_subprograma { printf("\n  cabs_subprgs -> cab_subprg ;");}
                          | cabeceras_subprogramas_mas ';' cabecera_subprograma { printf("\n  cabs_subprgs -> cabs_subprgs cab_subprg ;");}
;                          

cabecera_funcion: FUNCION nombre FLECHA_DOBLE tipo_no_estructurado_o_nombre_tipo { printf("\n  cab_func -> FUNCION nombre_decl => tipo_no_str_o_nom"); }
                | FUNCION nombre declaracion_parametros FLECHA_DOBLE tipo_no_estructurado_o_nombre_tipo { printf("\n  cab_func -> FUNCION nombre_decl decl_params => tipo_no_str_o_nom"); }
;  

cabecera_procedimiento: PROCEDIMIENTO nombre { printf("\n  cab_proced -> PROCEDIMIENTO nombre_decl"); }
                      | PROCEDIMIENTO nombre declaracion_parametros { printf("\n  cab_proced -> PROCEDIMIENTO nombre_decl decl_params"); }
;

cabecera_constructor: CONSTRUCTOR nombre { printf("\n  cab_constr -> CONSTRUCTOR nombre_decl"); }
                    | CONSTRUCTOR nombre declaracion_parametros { printf("\n  cab_constr -> CONSTRUCTOR nombre_decl decl_params"); }
;

cabecera_destructor: DESTRUCTOR nombre { printf("\n  cab_destr -> DESTRUCTOR nombre_decl");} ;

declaracion_parametros: '(' lista_parametros_formales ')' { printf("\n  decl_params -> list_params_form"); };

lista_parametros_formales: parametros_formales { printf("\n  list_params_form -> params_forms"); }
                         | lista_parametros_formales ';' parametros_formales { printf("\n  list_params_form -> list_params_form params_forms"); }
;

parametros_formales: nombres_mas ':' tipo_no_estructurado_o_nombre_tipo { printf("\n  params_forms -> nombres_decls : tipo_no_str_o_nom"); } 
                   | nombres_mas ':' tipo_no_estructurado_o_nombre_tipo MODIFICABLE { printf("\n  params_forms -> nombres_decls : tipo_no_str_o_nom MODIFICABLE"); } 
;

bloque_subprograma: bloque_instrucciones { printf("\n  blq_subprg -> blq_instr"); }
                  | declaracion_tipos bloque_instrucciones { printf("\n  blq_subprg -> decl_tip blq_instr"); }
                  | declaracion_constantes bloque_instrucciones { printf("\n  blq_subprg -> decl_const blq_instr"); }
                  | declaracion_variables bloque_instrucciones { printf("\n  blq_subprg -> decl_var blq_instr"); }
                  | declaracion_tipos declaracion_variables bloque_instrucciones { printf("\n  blq_subprg -> decl_tip decl_var blq_instr"); }
                  | declaracion_tipos declaracion_constantes bloque_instrucciones { printf("\n  blq_subprg -> decl_tip decl_const blq_instr"); }
                  | declaracion_constantes declaracion_variables bloque_instrucciones { printf("\n  blq_subprg -> decl_const decl_var blq_instr"); }
                  | declaracion_tipos declaracion_constantes declaracion_variables bloque_instrucciones { printf("\n  blq_subprg -> decl_tip decl_const decl_ var blq_instr"); }
;     

/*---------------------------------------------------------------------*/


/*---------------------------INSTRUCCIONES-----------------------------*/

instruccion: ';' { printf("\n  instr -> ;"); }
           | instruccion_asignacion { printf("\n  instr -> instr_asig"); }
           | instruccion_salir { printf("\n  instr -> instr_salir"); }
           | instruccion_devolver { printf("\n  instr -> instr_devol"); }
           | instruccion_llamada { printf("\n  instr -> instr_llmda"); }
           | instruccion_si { printf("\n  instr -> instr_si");}
           | instruccion_casos { printf("\n  instr -> instr_casos");}
           | instruccion_bucle { printf("\n  instr -> instr_bucle");}
           | instruccion_probar_excepto { printf("\n  instr -> instr_prob_exc");}
           | instruccion_lanzar { printf("\n  instr -> instr_lanzar");}
;

instrucciones: instruccion { printf("\n  instrs -> instr");}
             | instrucciones instruccion { printf("\n  instrs -> instrs instr");}
;             

instruccion_asignacion: objeto '=' expresion ';' {printf("\n  instr_asig -> objeto = expr ;"); } ; 

instruccion_salir: SALIR ';' {printf("\n  instr_salir -> SALIR ;"); }
                 | SALIR SI expresion ';' {printf("\n  instr_salir -> SALIR SI expr ;"); }
;                 

instruccion_devolver: DEVOLVER ';' {printf("\n  instr_devol -> DEVOLVER ;"); }
                    | DEVOLVER expresion ';' {printf("\n  instr_devol -> DEVOLVER expr ;"); }
;
      
instruccion_llamada: llamada_subprograma ';' {printf("\n  instr_llmda -> llmda_subprg ;"); } ;

llamada_subprograma: nombre '('                 ')' {printf("\n  llmda_subprg -> nombre_decl '('       ')' "); }
                   | nombre '(' expresiones_mas ')' {printf("\n  llmda_subprg -> nombre_decl '(' exprs ')' "); }
;                   

instruccion_si: SI expresion ENTONCES bloque_instrucciones { printf("\n  instr_si -> SI expr ENTONCES blq_instr"); }
              | SI expresion ENTONCES bloque_instrucciones SINO bloque_instrucciones { printf("\n  instr_si -> SI expr ENTONCES blq_instr SINO blq_instr"); }
;

instruccion_casos: EN CASO expresion SEA casos ';' { printf("\n  instr_casos -> EN CASO expr casos ;"); } ;

caso: CUANDO entradas FLECHA_DOBLE bloque_instrucciones { printf("\n  caso -> entrada => blq_instr"); };

casos: caso { printf("\n  casos -> caso"); }
     | casos caso { printf("\n  casos -> casos caso"); }
;   

entradas: entrada { printf("\n  entradas -> entrada"); }
        | entradas '|' entrada  { printf("\n  entradas -> entradas | entrada"); }
;     

entrada: expresion { printf("\n  entrada -> expr"); }
       | rango { printf("\n  entrada -> rango"); }  
       | OTRO { printf("\n  entrada -> OTRO"); }
;       

instruccion_bucle: clausula_iteracion bloque_instrucciones { printf("\n  instr_bucle -> claus_iter blq_instr"); } ;

clausula_iteracion: PARA nombre EN objeto { printf("\n  claus_iter -> PARA nombre_decl EN objeto"); }
                  | REPITE ELEMENTO nombre EN rango { printf("\n  claus_iter -> REPITE ELEMENTO nombre_decl EN rango"); } 
                  | REPITE ELEMENTO nombre EN rango DESCENDENTE { printf("\n  claus_iter -> REPITE ELEMENTO nombre_decl EN rango DESCENDENTE"); }
                  | MIENTRAS expresion { printf("\n  claus_iter -> MIENTRAS expr"); }
                  | REPITE HASTA expresion { printf("\n  claus_iter -> REPITE HASTA expr"); }
;  

instruccion_probar_excepto: PROBAR bloque_instrucciones EXCEPTO clausulas_excepciones { printf("\n  instr_prob_exc -> PROBAR blq_instr EXCEPTO clauss_excs"); }
                          | PROBAR bloque_instrucciones EXCEPTO clausulas_excepciones FINALMENTE bloque_instrucciones { printf("\n  instr_prob_exc -> PROBAR blq_instr EXCEPTO claus_excs FINALMENTE blq_instr"); }
;                          

clausula_excepcion: CUANDO nombre EJECUTA bloque_instrucciones { printf("\n  claus_exc -> CUANDO nombre_decl EJECUTA blq_instr"); } ;

/* Una o más clausulas excepciones */
clausulas_excepciones: clausula_excepcion { printf("\n  clauss_excs -> claus_exc"); }
                     | clausulas_excepciones clausula_excepcion { printf("\n  clauss_excs -> clauss_excs claus_exc"); }
;      

instruccion_lanzar: LANZAR nombre ';' {printf("\n  instr_lanzar -> LANZAR nombre_decl ;"); } ;

/*---------------------------------------------------------------------*/


/*-----------------------------EXPRESIONES-----------------------------*/

/*expresion: expresion_primaria { printf("\n  expr -> expr_prim"); }
         | expresion OR expresion { printf("\n  expr -> expr \\/ expr"); }
         | expresion AND expresion { printf("\n  expr -> expr /\\ expr"); }
         | '!' expresion { printf("\n  expr -> ! expr"); }
         | expresion '<' expresion { printf("\n  expr -> expr < expr"); }
         | expresion '>' expresion { printf("\n  expr -> expr > expr"); }
         | expresion LEQ expresion { printf("\n  expr -> expr =< expr"); }
         | expresion GEQ expresion { printf("\n  expr -> expr >= expr"); }
         | expresion EQ expresion { printf("\n  expr -> expr := expr"); }
         | expresion NEQ expresion { printf("\n  expr -> expr != expr"); }
         | expresion '@' expresion { printf("\n  expr -> expr @ expr"); }
         | expresion '~' expresion { printf("\n  expr -> expr ~ expr"); }
         | expresion '&' expresion { printf("\n  expr -> expr & expr"); }
         | expresion DESPI expresion { printf("\n  expr -> expr <- expr"); }
         | expresion DESPD expresion { printf("\n  expr -> expr -> expr"); }
         | expresion '+' expresion { printf("\n  expr -> expr + expr"); }
         | expresion '-' expresion { printf("\n  expr -> expr - expr"); }
         | expresion '*' expresion { printf("\n  expr -> expr * expr"); }
         | expresion '/' expresion { printf("\n  expr -> expr / expr"); }
         | expresion '%' expresion { printf("\n  expr -> expr % expr"); }
         | expresion POTENCIA expresion { printf("\n  expr -> expr ** expr"); }
         | '-' expresion %prec UNARIO { printf("\n  expr -> - expr"); }
;         
*/

expresion: expresion_or 

expresion_or: expresion_and OR expresion_or { printf("\n  expr -> expr /\\ expr"); }
            | expresion_and
;

expresion_and: expresion_negacion AND expresion_and { printf("\n  expr -> expr \\/ expr"); }
             | expresion_negacion
;

expresion_negacion: '!' expresion_primaria { printf("\n  expr -> ! expr_prim"); }
                  | expresion_comparacion
;                  

expresion_comparacion: expresion_comparacion '<' expresion_xorB { printf("\n  expr -> expr < expr"); }
                     | expresion_comparacion '>' expresion_xorB { printf("\n  expr -> expr > expr"); }
                     | expresion_comparacion LEQ expresion_xorB { printf("\n  expr -> expr =< expr"); }
                     | expresion_comparacion GEQ expresion_xorB { printf("\n  expr -> expr >= expr"); }
                     | expresion_comparacion EQ expresion_xorB { printf("\n  expr -> expr := expr"); }
                     | expresion_comparacion NEQ expresion_xorB { printf("\n  expr -> expr != expr"); }
                     | expresion_xorB
;

expresion_xorB: expresion_xorB '@' expresion_orB  { printf("\n  expr -> expr @ expr"); }
              | expresion_orB
;

expresion_orB: expresion_orB '~' expresion_andB { printf("\n  expr -> expr ~ expr"); }
             | expresion_andB
;

expresion_andB: expresion_andB '&' expresion_despl { printf("\n  expr -> expr & expr"); }
              | expresion_despl
;

expresion_despl: expresion_despl DESPI expresion_suma { printf("\n  expr -> expr <- expr"); }
               | expresion_despl DESPD expresion_suma { printf("\n  expr -> expr -> expr"); }
               | expresion_suma
;               

expresion_suma: expresion_suma '+' expresion_mult { printf("\n  expr -> expr + expr"); }
              | expresion_suma '-' expresion_mult { printf("\n  expr -> expr - expr"); }
              | expresion_mult
;              

expresion_mult: expresion_mult '*' expresion_potencia { printf("\n  expr -> expr * expr"); }
              | expresion_mult '/' expresion_potencia { printf("\n  expr -> expr / expr"); }
              | expresion_mult '%' expresion_potencia { printf("\n  expr -> expr % expr"); }
              | expresion_potencia
;       

expresion_potencia: expresion_unario POTENCIA expresion_potencia { printf("\n  expr -> expr ** expr"); }
                  | expresion_unario
;

expresion_unario: '-' expresion_primaria { printf("\n  expr -> - expr_prim"); }
                | expresion_primaria { printf("\n  expr -> expr_prim"); }
;                
         
/* Una o mas expresiones separadas por coma */
expresiones_mas: expresion { printf("\n  exprs -> expr"); }
               | expresiones_mas ',' expresion { printf("\n  exprs -> exprs ',' expr"); }
;      

expresion_primaria: expresion_constante { printf("\n  expr_prim -> expr_const"); }
                  | objeto { printf("\n  expr_prim -> obj"); }
                  | llamada_subprograma { printf("\n  expr_prim -> llmda_subprg"); }
                  | '(' expresion ')' { printf("\n  expr_prim -> ( expr )"); }      
                  | error { yyerrok; }            
;

objeto: nombre { printf("\n  obj -> nombre_decl"); }
      | objeto '[' expresiones_mas ']' {printf("\n  obj -> obj '[' exprs ']'"); }
      | objeto '.' IDENTIFICADOR {printf("\n  obj -> obj '.' IDENTIFICADOR"); }
;

expresion_constante: CTC_ENTERA { printf("\n  expr_const -> CTC_ENTERA"); }
                   | CTC_REAL { printf("\n  expr_const -> CTC_REAL"); }
                   | CTC_CADENA { printf("\n  expr_const -> CTC_CADENA"); }
                   | CTC_CARACTER { printf("\n  expr_const -> CTC_CARACTER"); }
                   | CTC_BOOLEANA { printf("\n  expr_const -> CTC_BOOLEANA"); }    
;

expresiones_constantes_mas: expresion_constante { printf("\n  exprs_consts -> expr_const"); }
                          | expresiones_constantes_mas ',' expresion_constante { printf("\n  exprs_consts -> exprs_consts , expr_const"); }                       
;

/*-------------------------------------------------------------------- */

%%

void yyerror(char *s) {
  fflush(stdout);
  printf("\n  ---------------------%s---------------------", s);
}

int yywrap() {
  return(1);
}

int main(int argc, char *argv[]) {
  yydebug = 0;

  if(argc < 2){
    printf("Uso: ./moronico NombreArchivo\n");
  }
  else{
    yyin = fopen(argv[1],"r");
    yyparse();
  }

  printf("\n");
}