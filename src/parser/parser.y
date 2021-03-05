/* C Parser */

%{
	#include "parser_common.h"
	#include "lexer/lexer_common.h"
    #define RETHDR(in,out) out=(struct astnode_hdr*)in;
%}
//For debugging
%define parse.trace
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

    /* Expressions */
%type  <hdr> expression
%type  <hdr> primary-expression
%type  <hdr> postfix-expression
%type  <lst> argument-expression-list
%type  <hdr> unary-expression
%type  <lexNode> '=' '&' '*' '+' '-' '~'
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

    /* Declarations */
%type  <hdr> declaration
%type  <hdr> declaration-specifiers
%type  <hdr> declaration-specifier
%type  <hdr> init-declarator-list
%type  <hdr> init-declarator
%type  <lexNode> storage-class-specifier
%type  <hdr> type-specifier
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
%type  <specInter> direct-declarator
%type  <specInter> pointer
%type  <ptr> type-qualifier-list
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


%initial-action {
    //Space to perform initialization actions at beginning of yyparse()
    //Init file scope symbol table here
    currTab = symtabCreate(SCOPE_FILE, TAB_GENERIC);
    currDecl.generic = allocEntry(ENTRY_GENERIC, true);
};


%% /* Grammar */

    /* 6.9 - External definitions */
translation-unit:
        external-declaration
    |   translation-unit external-declaration
    ;

external-declaration:
       /* function-definition
    |*/   declaration
    |     statement
    ;

    /* 6.9.1 - Function definitions */
    /* TODO: KR style definitions excluded from assg 3 */
/*function-definition:
        declaration-specifiers declarator compound-statement
    ;*/

    /* 6.8 - Statements and blocks */
statement:
        compound-statement
    |   expression-statement
    ;

    /* 6.8.2 - Compound statements */
    /* TODO: deal with scopes */
compound-statement:
        '{' block-item-list '}'
    |   '{' '}'
    ;

block-item-list:
        block-item
    |   block-item-list block-item
    ;

block-item:
        declaration
    |   statement
    ;

    /* 6.8.3 - Expression and null statements */
expression-statement:
        ';'
    |   expression ';'            {printAst($1, 0);}
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
        '='        {$$ = $1;}
    |   TIMESEQ    {$$ = $1;}
    |   DIVEQ      {$$ = $1;}
    |   MODEQ      {$$ = $1;}
    |   PLUSEQ     {$$ = $1;}
    |   MINUSEQ    {$$ = $1;}
    |   SHLEQ      {$$ = $1;}
    |   SHREQ      {$$ = $1;}
    |   ANDEQ      {$$ = $1;}
    |   XOREQ      {$$ = $1;}
    |   OREQ       {$$ = $1;}
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
        '&'    {$$ = $1;}
    |   '*'    {$$ = $1;}
    |   '+'    {$$ = $1;}
    |   '-'    {$$ = $1;}
    |   '~'    {$$ = $1;}
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
        IDENT                   {$$ = (struct astnode_hdr*)$1;}
    |   NUMBER                  {$$ = (struct astnode_hdr*)$1;}
    |   CHARLIT                 {$$ = (struct astnode_hdr*)$1;}
    |   STRING                  {$$ = (struct astnode_hdr*)$1;}
    |   '(' expression ')'      {$$ = $2;} /* throw away parentheses */
    ;

    /* 6.7 - Declarations */
declaration:
        declaration-specifiers {finalizeSpecs(currDecl);}  init-declarator-list ';'     {clearEntry(currDecl);}
    |   declaration-specifiers ';'                          {clearEntry(currDecl);}
    ;

    /* No actions to perform here - set in global */
declaration-specifiers:
        declaration-specifier
    |   declaration-specifiers declaration-specifier
    ;

declaration-specifier:
    /* TODO: ensure at most one of each */
    /* TODO: adjustment made from spec - converted to list - check this is valid */
        storage-class-specifier     {setStgSpec(currDecl, currTab, $1);}
    |   type-specifier              {addTypeSpec(currDecl, $1);}
    |   type-qualifier              {addTypeQual(&currDecl.generic->type_spec->qtype, currDecl.generic->type_spec->type_quals, $1, false);}
    /* TODO: ignored for assign 3: |   function-specifier */
    ;

init-declarator-list:
        init-declarator                             {varEnter(currTab, currDecl);}
    |   init-declarator-list ',' init-declarator    {varEnter(currTab, currDecl);}
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
        VOID        {RETHDR($1,$$)}
    |   CHAR        {RETHDR($1,$$)}
    |   SHORT       {RETHDR($1,$$)}
    |   INT         {RETHDR($1,$$)}
    |   LONG        {RETHDR($1,$$)}
    |   FLOAT       {RETHDR($1,$$)}
    |   DOUBLE      {RETHDR($1,$$)}
    |   SIGNED      {RETHDR($1,$$)}
    |   UNSIGNED    {RETHDR($1,$$)}
    |   _BOOL       {RETHDR($1,$$)}
    |   _COMPLEX    {RETHDR($1,$$)}
    |   struct-or-union-specifier {$$ = $1;}
    /* TODO: ignored here: |   enum-specifier */
    /* TODO: Ignored here: |   typedef-name */
    ;

    /* 6.7.2.1 - Struct/union specifiers */
struct-or-union-specifier:
        struct-or-union '{'
                {$<hdr>$=genStruct($1, currTab, currDecl, (struct LexVal*)NULL, true);}
                struct-declaration-list '}'              {$<hdr>$ = $<hdr>3; printStruct($$); finalizeStruct($$); exitScope();}
    |   struct-or-union IDENT '{'
                {$<hdr>$=genStruct($1, currTab, currDecl, $2, true);}
                struct-declaration-list '}'              {$<hdr>$ = $<hdr>4; printStruct($$); finalizeStruct($$); exitScope();}
    |   struct-or-union IDENT                            {$$=genStruct($1, currTab, currDecl, $2, false); finalizeStruct($$);}
    ;

struct-or-union:
        STRUCT  {$$ = $1;}
    |   UNION   {$$ = $1;}
    ;

struct-declaration-list:
        struct-declaration
    |   struct-declaration-list struct-declaration
    ;

struct-declaration:
        specifier-qualifier-list struct-declarator-list ';' {clearEntry(currDecl);}
    ;

specifier-qualifier-list:
    /* TODO: not per spec */
        specifier-qualifier
    |   specifier-qualifier-list specifier-qualifier
    ;

specifier-qualifier:
        type-specifier              {addTypeSpec(currDecl, $1);}
    |   type-qualifier              {addTypeQual(&currDecl.generic->type_spec->qtype, currDecl.generic->type_spec->type_quals, $1, false);}
    ;

struct-declarator-list:
        struct-declarator                               {structMembEnter(currTab, currDecl);}
    |   struct-declarator-list ',' struct-declarator    {structMembEnter(currTab, currDecl);}
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
        CONST       {$$ = $1;}
    |   RESTRICT    {$$ = $1;}
    |   VOLATILE    {$$ = $1;}
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
        IDENT                                                                        {currDecl.generic->ident = $1; $$ = (struct astnode_spec_inter*)currDecl.generic;}
    |   '(' declarator {$<specInter>$ = currDecl.generic->type_spec->parent;} ')'    {$$ = $<specInter>3;}
    /* Not per spec, adjusted per assignment 3 */
    |   direct-declarator '[' NUMBER ']'                                             {$$ = allocAry($1, $3, currDecl, currTab);}
    |   direct-declarator '[' ']'                                                    {$$ = allocAry($1, (struct LexVal*)NULL, currDecl, currTab);}
    |   direct-declarator '(' parameter-type-list ')'
    |   direct-declarator '(' identifier-list ')'
    |   direct-declarator '(' ')'
    ;

pointer:
        '*'                                {$$ = setPtr((struct astnode_spec_inter*)allocPtr(), (struct astnode_spec_inter*)currDecl.generic->type_spec, currDecl);}
    |   '*' type-qualifier-list            {$$ = setPtr((struct astnode_spec_inter*)$2, (struct astnode_spec_inter*)currDecl.generic->type_spec, currDecl);}
    |   pointer '*' type-qualifier-list    {$$ = setPtr((struct astnode_spec_inter*)$3, $1, currDecl);}
    |   pointer '*'                        {$$ = setPtr((struct astnode_spec_inter*)allocPtr(),$1 , currDecl);}
    ;

type-qualifier-list:
        type-qualifier                        {$$ = allocPtr(); addTypeQual(&$$->qtype, $$->type_quals, $1, true);}
    |   type-qualifier-list type-qualifier    {$$ = $1; addTypeQual(&$$->qtype, $$->type_quals, $2, true);}
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

    /*6.7.8 - Initialization */
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
