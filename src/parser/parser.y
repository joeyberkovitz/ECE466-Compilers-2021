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
%token <lexNode> TYPEDEF
%token <lexNode> EXTERN
%token <lexNode> STATIC
%token <lexNode> AUTO
%token <lexNode> REGISTER
%token <lexNode> VOID
%token <lexNode> CHAR
%token <lexNode> SHORT
%token <lexNode> INT
%token <lexNode> LONG
%token <lexNode> FLOAT
%token <lexNode> DOUBLE
%token <lexNode> SIGNED
%token <lexNode> UNSIGNED
%token <lexNode> _BOOL
%token <lexNode> _COMPLEX
%token <lexNode> STRUCT
%token <lexNode> UNION
%token <lexNode> CONST
%token <lexNode> RESTRICT
%token <lexNode> VOLATILE
%token <lexNode> ELLIPSIS
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

    /* Declarations */
%type  <hdr> declaration
%type  <hdr> declaration-specifiers
%type  <hdr> declaration-specifier
%type  <hdr> init-declarator-list
%type  <hdr> init-declarator
%type  <lexNode> storage-class-specifier
%type  <lexNode> type-specifier
%type  <hdr> struct-or-union-specifier
%type  <lexNode> struct-or-union
%type  <hdr> struct-declaration-list
%type  <hdr> struct-declaration
%type  <hdr> specifier-qualifier-list
%type  <hdr> specifier-qualifier
%type  <hdr> struct-declarator-list
%type  <hdr> struct-declarator
%type  <lexNode> type-qualifier
%type  <hdr> declarator
%type  <hdr> direct-declarator
%type  <hdr> pointer
%type  <hdr> type-qualifier-list
%type  <hdr> parameter-type-list
%type  <hdr> parameter-list
%type  <hdr> parameter-declaration
%type  <hdr> identifier-list
%type  <hdr> type-name
%type  <hdr> abstract-declarator
%type  <hdr> direct-abstract-declarator
%type  <hdr> initializer
%type  <hdr> initializer-list
%type  <hdr> designation
%type  <hdr> designator-list
%type  <hdr> designator

%type  <lexNode> assignment-operator
%type  <lexNode> '=' '&' '*' '+' '-' '~'

%initial-action {
    //Space to perform initialization actions at beginning of yyparse()
    //Init file scope symbol table here
};


%% /* Grammar */

    /* Program consists of series of expression statements */
program:
    program-line
    | program program-line
    ;

program-line:
        expression-statement
    |   declaration
    ;


    /* 6.8.3 - Expressions and null statements */
expression-statement:
    ';'
    | expression ';'            {printAst($1, 0);}
    ;

    /* TODO: 6.6 - Constant expressions; once we do declarations */
    /* 6.6 - constant expressions */
constant-expression:
        conditional-expression
    ;


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
    |   postfix-expression '(' ')'                          {$$ = allocFunc($1, allocList(NULL));}
    |   postfix-expression '.' IDENT                        {$3->sym = IDENT; $$ = allocBinop($1, (struct astnode_hdr*)$3, '.');}
    |   postfix-expression INDSEL IDENT                     {$3->sym = IDENT; $$ = allocBinop(allocUnop($1, '*'), (struct astnode_hdr*)$3, '.');}
    |   postfix-expression PLUSPLUS                         {$$ = allocUnop($1, PLUSPLUS);}
    |   postfix-expression MINUSMINUS                       {$$ = allocUnop($1, MINUSMINUS);}
        /* TODO: compound literals; once we have type-names */
    |   '(' type-name ')' '{' initializer-list '}'
    |   '(' type-name ')' '{' initializer-list ',' '}'
;

argument-expression-list:
        assignment-expression                               {$$ = allocList($1); }
    |   argument-expression-list ',' assignment-expression  {$$ = $1; addToList($1, $3); }
    ;

    /* 6.5.1 - Primary expressions */
primary-expression:
        IDENT                   {$1->sym = IDENT; $$ = (struct astnode_hdr*)$1;}
    |   NUMBER                  {$1->sym = NUMBER; $$ = (struct astnode_hdr*)$1;}
    |   CHARLIT                 {$1->sym = CHARLIT; $$ = (struct astnode_hdr*)$1;}
    |   STRING                  {$1->sym = STRING; $$ = (struct astnode_hdr*)$1;}
    |   '(' expression ')'      {$$ = $2;} /* throw away parentheses */
    ;

    /* 6.7 - Declarations */
declaration:
        declaration-specifiers init-declarator-list ';'
    |   declaration-specifiers ';'
    ;

declaration-specifiers:
        declaration-specifier
    |   declaration-specifiers declaration-specifier
    ;

declaration-specifier:
    /* TODO: ensure at most one of each */
    /* TODO: adjustment made from spec - converted to list - check this is valid */
        storage-class-specifier
    |   type-specifier
    |   type-qualifier
    /* TODO: ignored for assign 3: |   function-specifier */
    ;

init-declarator-list:
        init-declarator
    |   init-declarator-list ',' init-declarator
    ;

init-declarator:
        declarator
    /* TODO: initialized declarator skipped for assignment 3: |   declarator '=' initializer */
    ;

    /* 6.7.1 - Storage class specifiers */
storage-class-specifier:
        TYPEDEF     {$$ = $1;}
    |   EXTERN      {$$ = $1;}
    |   STATIC      {$$ = $1;}
    |   AUTO        {$$ = $1;}
    |   REGISTER    {$$ = $1;}
    ;

    /* 6.7.2 - Type specifiers */
type-specifier:
        VOID        {$$ = $1;}
    |   CHAR        {$$ = $1;}
    |   SHORT       {$$ = $1;}
    |   INT         {$$ = $1;}
    |   LONG        {$$ = $1;}
    |   FLOAT       {$$ = $1;}
    |   DOUBLE      {$$ = $1;}
    |   SIGNED      {$$ = $1;}
    |   UNSIGNED    {$$ = $1;}
    |   _BOOL       {$$ = $1;}
    |   _COMPLEX    {$$ = $1;}
    |   struct-or-union-specifier
    /* TODO: ignored here: |   enum-specifier */
    /* TODO: Ignored here: |   typedef-name */
    ;

    /* 6.7.2.1 - Struct/union specifiers */
struct-or-union-specifier:
        struct-or-union '{' struct-declaration-list '}'
    |   struct-or-union IDENT '{' struct-declaration-list '}'
    |   struct-or-union IDENT
    ;

struct-or-union:
        STRUCT
    |   UNION
    ;

struct-declaration-list:
        struct-declaration
    |   struct-declaration-list struct-declaration
    ;

struct-declaration:
        specifier-qualifier-list struct-declarator-list ';'
    ;

specifier-qualifier-list:
    /* TODO: not per spec */
        specifier-qualifier
    |   specifier-qualifier-list specifier-qualifier
    ;

specifier-qualifier:
        type-specifier
    |   type-qualifier
    ;

struct-declarator-list:
        struct-declarator
    |   struct-declarator-list ',' struct-declarator
    ;

struct-declarator:
        declarator
    /* TODO: not supporting bitfields |   declarator ':' constant-expression */
    ;

    /* 6.7.2.2 - Enum Specifiers */
    /* Ignored for assign 3
enum-specifier:
        ENUM IDENT '{' enumerator-list '}'
    |   ENUM '{' enumerator-list '}'
    |   ENUM IDENT '{' enumerator-list ',' '}'
    |   ENUM '{' enumerator-list ',' '}'
    |   ENUM IDENT
    ;

enumerator-list:
        enumerator
    |   enumerator-list ',' enumerator
    ;

enumerator:
        enumeration-constant
    |   enumeration-constant '=' constant-expression
    ; */

    /* 6.7.3 - Type Qualifiers */
type-qualifier:
        CONST
    |   RESTRICT
    |   VOLATILE
    ;


    /* 6.7.4 - Function Specifiers */
/* TODO: Ignored  for assign 3
function-specifier:
    INLINE
; */

    /* 6.7.5 - Declarators */
declarator:
        pointer direct-declarator
    |   direct-declarator
    ;

direct-declarator:
        IDENT
    |   '(' declarator ')'
    /* Not per spec, adjusted per assignment 3 */
    |   direct-declarator '[' NUMBER ']'
    |   direct-declarator '[' ']'
    |   direct-declarator '(' parameter-type-list ')'
    |   direct-declarator '(' identifier-list ')'
    |   direct-declarator '(' ')'
    ;

pointer:
        '*'
    |   '*' type-qualifier-list
    |   '*' type-qualifier-list pointer
    |   '*' pointer
    ;

type-qualifier-list:
        type-qualifier
    |   type-qualifier-list type-qualifier
    ;

parameter-type-list:
        parameter-list
    |   parameter-list ',' ELLIPSIS
    ;

parameter-list:
        parameter-declaration
    |   parameter-list ',' parameter-declaration
    ;

parameter-declaration:
        declaration-specifiers declarator
    |   declaration-specifiers abstract-declarator
    |   declaration-specifiers
    ;

identifier-list:
        IDENT
    |   identifier-list ',' IDENT
    ;

    /* 6.7.6 - Type names */
type-name:
        specifier-qualifier-list
    |   specifier-qualifier-list abstract-declarator
    ;

abstract-declarator:
        pointer
    |   pointer direct-abstract-declarator
    |   direct-abstract-declarator
    ;

direct-abstract-declarator:
        '(' abstract-declarator ')'
    |   direct-abstract-declarator '[' assignment-expression ']'
    |   '[' assignment-expression ']'
    |   direct-abstract-declarator '[' ']'
    |   '[' ']'
    |   direct-abstract-declarator '[' '*' ']'
    |   '[' '*' ']'
    |   direct-abstract-declarator '(' parameter-type-list ')'
    |   '(' parameter-type-list ')'
    |   direct-abstract-declarator '(' ')'
    |   '(' ')'
    ;

    /*TODO:  6.7.8 - skipped for assignment 3 */
initializer:
        assignment-expression
    |   '{' initializer-list '}'
    |   '{' initializer-list ',' '}'
    ;

initializer-list:
        designation initializer
    |   initializer
    |   initializer-list ',' designation initializer
    |   initializer-list ',' initializer
    ;

designation:
        designator-list '='
    ;

designator-list:
        designator
    |   designator-list designator
    ;

designator:
        '[' constant-expression ']'
    |   '.' IDENT
    ;


%% /* Code */
