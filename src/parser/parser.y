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
%token <lexNode> SIZEOF
%token <lexNode> SHL
%token <lexNode> SHR
%token <lexNode> LTEQ
%token <lexNode> GTEQ
%token <lexNode> EQEQ
%token <lexNode> NOTEQ
%token <lexNode> LOGAND
%token <lexNode> LOGOR
%token <lexNode> TIMESEQ
%token <lexNode> DIVEQ
%token <lexNode> MODEQ
%token <lexNode> PLUSEQ
%token <lexNode> MINUSEQ
%token <lexNode> SHLEQ
%token <lexNode> SHREQ
%token <lexNode> ANDEQ
%token <lexNode> XOREQ
%token <lexNode> OREQ
%type  <hdr> expression
%type  <hdr> primary-expression
%type  <hdr> postfix-expression
%type  <lst> argument-expression-list
%type  <hdr> unary-expression
%type  <lexNode> unary-operator
%type  <hdr> cast-expression
%type  <hdr> multiplicative-expression
%type  <hdr> additive-expression
%type  <hdr> shift-expression
%type  <hdr> relational-expression
%type  <hdr> equality-expression
%type  <hdr> AND-expression
%type  <hdr> exclusive-OR-expression
%type  <hdr> inclusive-OR-expression
%type  <hdr> logical-AND-expression
%type  <hdr> logical-OR-expression
%type  <hdr> conditional-expression
%type  <hdr> assignment-expression
%type  <lexNode> assignment-operator
%type  <lexNode> '=' '&' '*' '+' '-' '~'

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

    /* TODO: 6.6 - Constant expressions; once we do declarations */

    /* 6.5.17 - Comma expressions */
expression:
        assignment-expression                {$$ = $1;}
    |   expression ',' assignment-expression {$$ = allocBinop($1, $3, ',');}
    ;

    /* 6.5.16 - Assignment expressions */
assignment-expression:
        conditional-expression                                     {$$ = $1;}
    |   unary-expression assignment-operator assignment-expression {$$ = allocAssignment($1, $3, $2);}
    ;

assignment-operator:
        '='        {$1->sym = '='; $$ = $1;}
    |   TIMESEQ    {$1->sym = TIMESEQ; $$ = $1;}
    |   DIVEQ      {$1->sym = DIVEQ; $$ = $1;}
    |   MODEQ      {$1->sym = MODEQ; $$ = $1;}
    |   PLUSEQ     {$1->sym = PLUSEQ; $$ = $1;}
    |   MINUSEQ    {$1->sym = MINUSEQ; $$ = $1;}
    |   SHLEQ      {$1->sym = SHLEQ; $$ = $1;}
    |   SHREQ      {$1->sym = SHREQ; $$ = $1;}
    |   ANDEQ      {$1->sym = ANDEQ; $$ = $1;}
    |   XOREQ      {$1->sym = XOREQ; $$ = $1;}
    |   OREQ       {$1->sym = OREQ; $$ = $1;}
    ;

    /* 6.5.15 - Conditional expressions */
conditional-expression:
        logical-OR-expression                                           {$$ = $1;}
    |   logical-OR-expression '?' expression ':' conditional-expression {$$ = allocTerop($1, $3, $5);}
    ;

    /* 6.5.14 - Logical OR expressions */
logical-OR-expression:
        logical-AND-expression                             {$$ = $1;}
    |   logical-OR-expression LOGOR logical-AND-expression {$$ = allocBinop($1, $3, LOGOR);}
    ;

    /* 6.5.13 - Logical AND expressions */
logical-AND-expression:
        inclusive-OR-expression                               {$$ = $1;}
    |   logical-AND-expression LOGAND inclusive-OR-expression {$$ = allocBinop($1, $3, LOGAND);}
    ;

    /* 6.5.12 - Bitwise inclusive OR expressions */
inclusive-OR-expression:
        exclusive-OR-expression                             {$$ = $1;}
    |   inclusive-OR-expression '|' exclusive-OR-expression {$$ = allocBinop($1, $3, '|');}
    ;

    /* 6.5.11 - Bitwise exclusive OR expressions */
exclusive-OR-expression:
        AND-expression                             {$$ = $1;}
    |   exclusive-OR-expression '^' AND-expression {$$ = allocBinop($1, $3, '^');}
    ;

    /* 6.5.10 - Bitwise AND expressions */
AND-expression:
        equality-expression                    {$$ = $1;}
    |   AND-expression '&' equality-expression {$$ = allocBinop($1, $3, '&');}
    ;

    /* 6.5.9 - Equality expressions */
equality-expression:
        relational-expression                           {$$ = $1;}
    |   equality-expression EQEQ relational-expression  {$$ = allocBinop($1, $3, EQEQ);}
    |   equality-expression NOTEQ relational-expression {$$ = allocBinop($1, $3, NOTEQ);}
    ;

    /* 6.5.8 - Relational expressions */
relational-expression:
        shift-expression                            {$$ = $1;}
    |   relational-expression '<' shift-expression  {$$ = allocBinop($1, $3, '<');}
    |   relational-expression '>' shift-expression  {$$ = allocBinop($1, $3, '>');}
    |   relational-expression LTEQ shift-expression {$$ = allocBinop($1, $3, LTEQ);}
    |   relational-expression GTEQ shift-expression {$$ = allocBinop($1, $3, GTEQ);}
    ;

    /* 6.5.7 - Shift expressions */
shift-expression:
        additive-expression                      {$$ = $1;}
    |   shift-expression SHL additive-expression {$$ = allocBinop($1, $3, SHL);}
    |   shift-expression SHR additive-expression {$$ = allocBinop($1, $3, SHR);}
    ;

    /* 6.5.6 - Additive expressions */
additive-expression:
        multiplicative-expression                         {$$ = $1;}
    |   additive-expression '+' multiplicative-expression {$$ = allocBinop($1, $3, '+');}
    |   additive-expression '-' multiplicative-expression {$$ = allocBinop($1, $3, '-');}
    ;

    /* 6.5.5 - Multiplicative expressions */
multiplicative-expression:
        cast-expression                               {$$ = $1;}
    |   multiplicative-expression '*' cast-expression {$$ = allocBinop($1, $3, '*');}
    |   multiplicative-expression '/' cast-expression {$$ = allocBinop($1, $3, '/');}
    |   multiplicative-expression '%' cast-expression {$$ = allocBinop($1, $3, '%');}
    ;

    /* 6.5.4 - Cast expressions */
cast-expression:
        unary-expression    {$$ = $1;}
        /* TODO: type casts; once we have type-names */
    ;

    /* 6.5.3 - Unary expressions */
unary-expression:
        postfix-expression             {$$ = $1;}
    |   PLUSPLUS unary-expression      {$$ = allocPostIncDec($1, $2, '+');}
    |   MINUSMINUS unary-expression    {$$ = allocPostIncDec($1, $2, '-');}
    |   unary-operator cast-expression {$$ = allocUnop($2, $1->sym);}
    |   SIZEOF unary-expression        {$$ = allocUnop($2, SIZEOF);}
        /* TODO: sizeof type-name; once we have type-names */
    ;

unary-operator:
        '&'    {$1->sym = '&'; $$ = $1;}
    |   '*'    {$1->sym = '*'; $$ = $1;}
    |   '+'    {$1->sym = '+'; $$ = $1;}
    |   '-'    {$1->sym = '-'; $$ = $1;}
    |   '~'    {$1->sym = '~'; $$ = $1;}
    ;

    /* 6.5.2 - Postfix expressions */
postfix-expression:
        primary-expression                                  {$$ = $1;}
        /* TODO: check types; Array subscripting: $1 should be pointer, $3 should be offset */
    |   postfix-expression '[' expression ']'               {$$ = allocBinop($1, $3, '+');}
    |   postfix-expression '(' argument-expression-list ')' {$$ = allocFunc($1, $3);}
    |   postfix-expression '.' IDENT                        {$3->sym = IDENT; $$ = allocBinop($1, (struct astnode_hdr*)$3, '.');}
    |   postfix-expression INDSEL IDENT                     {$3->sym = IDENT; $$ = allocBinop(allocUnop($1, '*'), (struct astnode_hdr*)$3, '.');}
    |   postfix-expression PLUSPLUS                         {$$ = allocUnop($1, PLUSPLUS);}
    |   postfix-expression MINUSMINUS                       {$$ = allocUnop($1, MINUSMINUS);}
        /* TODO: compound literals; once we have type-names */
    ;

argument-expression-list:
        assignment-expression                               {$$ = allocList($1); }
    |   argument-expression-list ',' assignment-expression  {$$ = $1; addToList($1, $3); }
        /*TODO: Possible edge case - list may already exist and this is an empty element */
    |                                                       {$$ = allocList(NULL);}
    ;

    /* 6.5.1 - Primary expressions */
primary-expression:
        IDENT                   {$1->sym = IDENT; $$ = (struct astnode_hdr*)$1;}
    |   NUMBER                  {$1->sym = NUMBER; $$ = (struct astnode_hdr*)$1;}
    |   CHARLIT                 {$1->sym = CHARLIT; $$ = (struct astnode_hdr*)$1;}
    |   STRING                  {$1->sym = STRING; $$ = (struct astnode_hdr*)$1;}
    |   '(' expression ')'      {$$ = $2;} /* throw away parentheses */
    ;

%% /* Code */
