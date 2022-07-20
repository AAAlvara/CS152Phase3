  /* cs152-miniL phase2 */
%{
  #include <stdio.h>
  #include <stdlib.h>
void yyerror(const char *msg);
extern int currLine;
extern int currPos;
extern int yylex(void);
FILE * yyin;
%}



%union{
  /* put your types here */
  char* id_val;
  int num_val;
}

%error-verbose
%locations

/* %start program */
%start prog_start
%token FUNCTION BEGIN_PARAMS END_PARAMS BEGIN_LOCALS END_LOCALS BEGIN_BODY END_BODY INTEGER ARRAY ENUM OF IF THEN ENDIF ELSE FOR WHILE DO BEGINLOOP ENDLOOP CONTINUE READ WRITE AND OR NOT TRUE FALSE RETURN SUB ADD MULT DIV MOD EQ NEQ LT GT LTE GTE SEMICOLON COLON COMMA L_PAREN R_PAREN L_SQUARE_BRACKET R_SQUARE_BRACKET ASSIGN
%token <id_val> IDENT
%token <num_val> NUMBER

%right ASSIGN
%left OR
%left AND
%left NOT
%left NEQ EQ GTE GT LTE LT
%left ADD SUB

%left MULT DIV MOD

%right UMINUS

%left L_SQUARE_BRACKET R_SQUARE_BRACKET

%left L_PAREN R_PAREN


%% 

  /* write your rules here */

  prog_start: functions {printf("prog_start -> functions\n");}
            ;

functions: /*epsilon*/ {printf("functions -> epsilon\n");}
         | function functions {printf("functions -> function functions\n");}
         ;


function:    FUNCTION IDENT SEMICOLON BEGIN_PARAMS declarations END_PARAMS BEGIN_LOCALS declarations END_LOCALS BEGIN_BODY statements END_BODY
{printf("function -> FUNCTION IDENT %s SEMMICOLON BEGIN_PARAMS declarations END_PARAMS BEGIN_LOCALS declarations END_LOCALS BEGIN_BODY statements END_BODY/n");}
;


declarations: /*epsilon*/ {printf("declarations -> epsilon\n");}
            | declaration SEMICOLON declarations {printf("declarations -> declaration SEMICOLON declarations\n");}
            ;

declaration: identifiers COLON INTEGER {printf("declaration -> identifiers COLON INTEGER\n");}
            | identifiers COLON ENUM L_PAREN identifiers R_PAREN {printf("declaration -> identifiers COLON ENUM L_PAREN identifiers R_PAREN\n");}
            | identifiers COLON ARRAY L_SQUARE_BRACKET NUMBER R_SQUARE_BRACKET OF INTEGER {printf("declaration -> identifiers COLON ARRAY L_SQUARE_BRACKET %d R_SQUARE_BRACKET OF INTEGER\n");}
;
identifiers: IDENT {printf("identifiers -> IDENT %s\n");}
            | IDENT COMMA identifiers {printf("identifiers -> IDENT %s COMMA identifiers \n");}
;


statements: statement SEMICOLON {printf("statements -> statement SEMICOLON\n");}
            | statement SEMICOLON statements {printf("statements -> statement SEMICOLON statements\n");}
            ;

statement: var ASSIGN expression {printf("statement -> var Assign expression\n");}
       | IF bool_expression THEN statements ENDIF {printf("statement -> IF bool_expression THEN statements ENDIF\n");}
        | IF bool_expression THEN statements ELSE statements ENDIF {printf("statement -> IF bool_expression THEN statements ELSE statements ENDIF\n");}
        | WHILE bool_expression BEGINLOOP statements ENDLOOP {printf("statement -> WHILE bool_expression BEGINLOOP statements ENDLOOP\n");}
        | DO BEGINLOOP statements ENDLOOP WHILE bool_expression {printf("statement -> DO BEGINLOOP statements ENDLOOP\n");}
        | FOR variables ASSIGN NUMBER SEMICOLON bool_expression SEMICOLON variables ASSIGN expression BEGINLOOP statements ENDLOOP {printf("statement -> FOR variables ASSIGN %d SEMICOLON bool_expression SEMICOLON variables ASSIGN expression BEGINLOOP statements ENDLOOP\n");}
        | READ variables {printf("statement -> READ variables\n");}
        | WRITE variables {printf("statement -> WRITE variables\n");}
        | CONTINUE  {printf("statement -> CONTINUE\n");}
        | RETURN expression {printf("statement -> RETURN expression\n");}
        ;



bool_expression: relation_and_expr {printf("bool_expression -> relation_and_expr\n");}
         | bool_expression OR relation_and_expr {printf("bool_expression -> bool_expression OR relation_and_expr\n");}
;


relation_and_expr: relation_expr {printf("relation_and_expr -> relation_expr\n");}
                  | relation_and_expr AND relation_expr {printf("relation_and_expr -> relation_and_expr AND relation_expr\n");}

;

relation_expr: expression comp expression {printf("relation_expr -> expression comp expression\n");}
		       | NOT expression comp expression {printf("relation_expr -> NOT expression comp expression\n");}
		       | TRUE {printf("relation_expr -> TRUE\n");}
      		 | NOT TRUE {printf("relation_expr -> NOT TRUE\n");}
      		 | FALSE {printf("relation_expr -> FALSE\n");}
	      	 | NOT FALSE {printf("relation_expr -> NOT FALSE\n");}
	      	 | L_PAREN bool_expression R_PAREN {printf("relation_expr -> L_PAREN bool_expression R_PAREN\n");}
		       | NOT L_PAREN bool_expression R_PAREN {printf("relation_expr -> NOT L_PAREN bool_expression R_PAREN\n");}
;  

comp: EQ {printf("comp -> EQ\n");}
	 | NEQ {printf("comp -> NEQ\n");}
	 | LT {printf("comp -> LT\n");}
	 | GT {printf("comp -> GT\n");}
	 | LTE {printf("comp -> LTE\n");}
	 | GTE {printf("comp -> GTE\n");}
;

expression: multiplicative_expr {printf("expression -> multiplicative_expr\n");}
	         | expression ADD multiplicative_expr {printf("expression -> expression ADD multiplicative_expr\n");}
	         | expression SUB multiplicative_expr {printf("expression -> expression SUB multiplicative_expr\n");}

;




multiplicative_expr: term {printf("multiplicative_expr -> term\n");}
	      | multiplicative_expr MULT term {printf("multiplicative_expr -> multiplicative_expr MULT term\n");}
	      | multiplicative_expr DIV term {printf("multiplicative_expr -> multiplicative_expr DIV term\n");}
	      | multiplicative_expr MOD term {printf("multiplicative_expr -> multiplicative_expr MOD term\n");}

;



term: var {printf("term -> var\n");}
    | SUB var {printf("term -> SUB var\n");}
	 | NUMBER {printf("term -> NUMBER %d\n");}
	 | SUB NUMBER {printf("term -> SUB NUMBER %d\n");}
	 | L_PAREN expression R_PAREN {printf("term -> L_PAREN expression R_PAREN\n");}
    | IDENT L_PAREN expression R_PAREN {printf("term -> IDENT %s L_PAREN expression R_PAREN\n");}
    | IDENT L_PAREN R_PAREN {printf("term -> IDENT %s L_PAREN R_PAREN\n");}
;

var: IDENT {printf("var -> IDNET %s\n");}
        | IDENT L_SQUARE_BRACKET expression R_SQUARE_BRACKET {printf("var -> IDNET %s R_SQUARE_BRACKET expression L_SQUARE_BRACKET\n", $1);}
;
variables : var COMMA variables {printf("variables -> COMMA variables\n");}
        | var {printf("variables -> var\n");}

;





%% 

int main(int argc, char **argv) {
  if(argc >= 2){
    yyin = fopen(argv[1],"r");
    if(yyin == NULL){
      yyin = stdin;
    }
  }
  else{
    yyin=stdin;
  }
   yyparse();
   return 0;
}

void yyerror(const char *msg) {
    /* implement your error handling */
    printf("ERROR: On Line: %d, position %d: %s\n", currLine, currPos,msg);
}