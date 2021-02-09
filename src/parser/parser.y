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
%token <lexNode> INDSEL
%token <lexNode> PLUSPLUS
%token <lexNode> MINUSMINUS
%type  <hdr> expression
%type  <hdr> primary-expression
%type  <hdr> postfix-expression
%type  <hdr> assignment-expression
%type  <lst> argument-expression-list

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
    postfix-expression
    ;


    /* 6.5.1 - Primary expressions */
primary-expression:
        IDENT                   {$$ = allocLexCtr($1, IDENT);}
    |   NUMBER                  {$$ = allocLexCtr($1, NUMBER);}
    |   CHARLIT                 {$$ = allocLexCtr($1, CHARLIT);}
    |   STRING                  {$$ = allocLexCtr($1, STRING);}
    |   '(' expression ')'      {$$ = $2;} /* throw away parentheses */
    ;

    /* 6.5.2 - Postfix expressions */
postfix-expression:
        primary-expression                      {$$ = $1;}
        /* TODO: check types; Array subscripting: $1 should be pointer, $3 should be offset */
    |   postfix-expression '[' expression ']'   {$$ = allocBinop($1, $3, '+');}
    |   postfix-expression '(' argument-expression-list ')' {$$ = allocFunc($1, $3);}
    |   postfix-expression '.' IDENT    {$$ = allocBinop($1, allocLexCtr($3, IDENT), '.');}
    |   postfix-expression INDSEL IDENT    {$$ = allocBinop(allocUnop($1, '*'), IDNET, '.');}
    |   postfix-expression PLUSPLUS    {$$ = allocUnop($1, PLUSPLUS);}
    |   postfix-expression MINUSMINUS    {$$ = allocUnop($1, MINUSMINUS);}
    |
    ;

argument-expression-list:
        assignment-expression                               {$$ = allocList($1); }
    |   argument-expression-list ',' assignment-expression  {$$ = $1; addToList($1, $3); }
    |                                                       {$$ = allocList(NULL);}
;

assignment-expression:
    expression;

%% /* Code */
