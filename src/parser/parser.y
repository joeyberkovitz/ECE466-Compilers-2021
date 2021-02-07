/* C Parser */

%{
	#include "parser_common.h"
	#include "lexer/lexer_common.h"
%}

%define api.value.type {union astnode}
%token <lexNode> IDENT
%token <lexNode> NUMBER
%token <lexNode> CHARLIT
%token <lexNode> STRING
%type  <hdr> expression
%type  <hdr> primary-expression

%% /* Grammar */

    /* Program consists of series of expression statements */
program:
    expression-statement
    | program expression-statement
    ;

    /* 6.8.3 - Expressions and null statements */
expression-statement:
    ';'
    | expression ';'            {printAst($1, 0);}
    ;

expression:
    primary-expression
    ;


    /* 6.5.1 - Primary expressions */
primary-expression:
        IDENT                   {printf("Found ident"); }
    |   NUMBER                  {printf("Found number"); }
    |   CHARLIT                 {printf("Found charlit"); }
    |   STRING                  {printf("Found string"); }
    |   '(' expression ')'      {printf("Found expr"); }
    ;


%% /* Code */