%option noyywrap
	/* Imports required by lexer */
%top{
	#include "ltests/tokens-manual.h"
	#include "lexer_common.c"
	
	extern struct LexVal yylval;
	
	int currLine = 1;
	char currFile[256] = "<stdin>"; //Linux limit is 255
}
	/* Definitions */
DIGIT		[0-9]
NONDIGIT	[A-Za-z_]
IDCHARS		[0-9A-Za-z_]
DECIMAL		[1-9][0-9]*
OCTAL		0[0-7]*
HEX			0[Xx][0-9A-Fa-f]+
ULONG		([Ll][Uu]|[Uu][Ll])
LONGLONG	(LL|ll)
ULONGLONG	((LL|ll)[Uu]|[Uu](LL|ll))

	/* States */
%x GCC_META
%x GCC_META_FILE
%x GCC_META_FILE_END
%x GCC_META_FLAGS
%%

^\#						BEGIN(GCC_META);
<GCC_META>{DIGIT}+		{currLine = atoi(yytext);}
<GCC_META>["]			BEGIN(GCC_META_FILE);
<GCC_META_FILE>.+/["]	{strncpy(currFile, yytext, 255); BEGIN(GCC_META_FILE_END);}
<GCC_META_FILE_END>["]	BEGIN(GCC_META_FLAGS);
<GCC_META_FLAGS>{DIGIT}	{;} /* TODO: maybe do something with flags */
<GCC_META_FLAGS>\n		BEGIN(INITIAL);


	/* Keywords - 6.4.1 */
auto				{processLine(&yylval); return AUTO;}
break				{processLine(&yylval); return BREAK;}
case				{processLine(&yylval); return CASE;}
char				{processLine(&yylval); return CHAR;}
const				{processLine(&yylval); return CONST;}
continue			{processLine(&yylval); return CONTINUE;}
default				{processLine(&yylval); return DEFAULT;}
do					{processLine(&yylval); return DO;}
double				{processLine(&yylval); return DOUBLE;}
else				{processLine(&yylval); return ELSE;}
enum				{processLine(&yylval); return ENUM;}
extern				{processLine(&yylval); return EXTERN;}
float				{processLine(&yylval); return FLOAT;}
for					{processLine(&yylval); return FOR;}
goto				{processLine(&yylval); return GOTO;}
if					{processLine(&yylval); return IF;}
inline				{processLine(&yylval); return INLINE;}
int					{processLine(&yylval); return INT;}
long				{processLine(&yylval); return LONG;}
register			{processLine(&yylval); return REGISTER;}
restrict			{processLine(&yylval); return RESTRICT;}
return				{processLine(&yylval); return RETURN;}
short				{processLine(&yylval); return SHORT;}
signed				{processLine(&yylval); return SIGNED;}
sizeof				{processLine(&yylval); return SIZEOF;}
static				{processLine(&yylval); return STATIC;}
struct				{processLine(&yylval); return STRUCT;}
switch				{processLine(&yylval); return SWITCH;}
typedef				{processLine(&yylval); return TYPEDEF;}
union				{processLine(&yylval); return UNION;}
unsigned			{processLine(&yylval); return UNSIGNED;}
void				{processLine(&yylval); return VOID;}
volatile			{processLine(&yylval); return VOLATILE;}
while				{processLine(&yylval); return WHILE;}
_Bool				{processLine(&yylval); return _BOOL;}
_Complex			{processLine(&yylval); return _COMPLEX;}
_Imaginary			{processLine(&yylval); return _IMAGINARY;}


	/* Identifiers - 6.4.2 */
{NONDIGIT}{IDCHARS}*	{setStr(&yylval, yytext, yyleng); processLine(&yylval); return IDENT;}

	/* Constants - 6.4.4 */
	/* Integers - 6.4.4.1 */
{DECIMAL}						{int base = 10; setInt(&yylval, yytext, 0, base); processLine(&yylval); return NUMBER;}
{DECIMAL}[uU]					{int base = 10; setInt(&yylval, yytext, uint_type, base); processLine(&yylval); return NUMBER;}
{DECIMAL}[lL]					{int base = 10; setInt(&yylval, yytext, lint_type, base); processLine(&yylval); return NUMBER;}
{DECIMAL}{ULONG}				{int base = 10; setInt(&yylval, yytext, ulint_type, base); processLine(&yylval); return NUMBER;}
{DECIMAL}{LONGLONG}				{int base = 10; setInt(&yylval, yytext, llint_type, base); processLine(&yylval); return NUMBER;}
{DECIMAL}{ULONGLONG}			{int base = 10; setInt(&yylval, yytext, ullint_type, base); processLine(&yylval); return NUMBER;}

{OCTAL}							{int base = 8; setInt(&yylval, yytext, 0, base); processLine(&yylval); return NUMBER;}
{OCTAL}[uU]						{int base = 8; setInt(&yylval, yytext, uint_type, base); processLine(&yylval); return NUMBER;}
{OCTAL}[lL]						{int base = 8; setInt(&yylval, yytext, lint_type, base); processLine(&yylval); return NUMBER;}
{OCTAL}{ULONG}					{int base = 8; setInt(&yylval, yytext, ulint_type, base); processLine(&yylval); return NUMBER;}
{OCTAL}{LONGLONG}				{int base = 8; setInt(&yylval, yytext, llint_type, base); processLine(&yylval); return NUMBER;}
{OCTAL}{ULONGLONG}				{int base = 8; setInt(&yylval, yytext, ullint_type, base); processLine(&yylval); return NUMBER;}

{HEX}							{int base = 16; setInt(&yylval, yytext, 0, base); processLine(&yylval); return NUMBER;}
{HEX}[uU]						{int base = 16; setInt(&yylval, yytext, uint_type, base); processLine(&yylval); return NUMBER;}
{HEX}[lL]						{int base = 16; setInt(&yylval, yytext, lint_type, base); processLine(&yylval); return NUMBER;}
{HEX}{ULONG}					{int base = 16; setInt(&yylval, yytext, ulint_type, base); processLine(&yylval); return NUMBER;}
{HEX}{LONGLONG}					{int base = 16; setInt(&yylval, yytext, llint_type, base); processLine(&yylval); return NUMBER;}
{HEX}{ULONGLONG}				{int base = 16; setInt(&yylval, yytext, ullint_type, base); processLine(&yylval); return NUMBER;}


\r?\n					{currLine++;}
<*>[ \t]				{;}
.						{fprintf(stderr, "%s:%d: Error: unrecognized character '%s'\n", currFile, currLine, yytext);}

%%
	/* Code to run */

int main() {
	int t;
	char *kw;
	while(t=yylex()){
		switch(t){
			//Keywords
			case AUTO: printf("%s\t%d\tAUTO\n", yylval.file, yylval.line); break;
			case BREAK: printf("%s\t%d\tBREAK\n", yylval.file, yylval.line); break;
			case CASE: printf("%s\t%d\tCASE\n", yylval.file, yylval.line); break;
			case CHAR: printf("%s\t%d\tCHAR\n", yylval.file, yylval.line); break;
			case CONST: printf("%s\t%d\tCONST\n", yylval.file, yylval.line); break;
			case CONTINUE: printf("%s\t%d\tCONTINUE\n", yylval.file, yylval.line); break;
			case DEFAULT: printf("%s\t%d\tDEFAULT\n", yylval.file, yylval.line); break;
			case DO: printf("%s\t%d\tDO\n", yylval.file, yylval.line); break;
			case DOUBLE: printf("%s\t%d\tDOUBLE\n", yylval.file, yylval.line); break;
			case ELSE: printf("%s\t%d\tELSE\n", yylval.file, yylval.line); break;
			case ENUM: printf("%s\t%d\tENUM\n", yylval.file, yylval.line); break;
			case EXTERN: printf("%s\t%d\tEXTERN\n", yylval.file, yylval.line); break;
			case FLOAT: printf("%s\t%d\tFLOAT\n", yylval.file, yylval.line); break;
			case FOR: printf("%s\t%d\tFOR\n", yylval.file, yylval.line); break;
			case GOTO: printf("%s\t%d\tGOTO\n", yylval.file, yylval.line); break;
			case IF: printf("%s\t%d\tIF\n", yylval.file, yylval.line); break;
			case INLINE: printf("%s\t%d\tINLINE\n", yylval.file, yylval.line); break;
			case INT: printf("%s\t%d\tINT\n", yylval.file, yylval.line); break;
			case LONG: printf("%s\t%d\tLONG\n", yylval.file, yylval.line); break;
			case REGISTER: printf("%s\t%d\tREGISTER\n", yylval.file, yylval.line); break;
			case RESTRICT: printf("%s\t%d\tRESTRICT\n", yylval.file, yylval.line); break;
			case RETURN: printf("%s\t%d\tRETURN\n", yylval.file, yylval.line); break;
			case SHORT: printf("%s\t%d\tSHORT\n", yylval.file, yylval.line); break;
			case SIGNED: printf("%s\t%d\tSIGNED\n", yylval.file, yylval.line); break;
			case SIZEOF: printf("%s\t%d\tSIZEOF\n", yylval.file, yylval.line); break;
			case STATIC: printf("%s\t%d\tSTATIC\n", yylval.file, yylval.line); break;
			case STRUCT: printf("%s\t%d\tSTRUCT\n", yylval.file, yylval.line); break;
			case SWITCH: printf("%s\t%d\tSWITCH\n", yylval.file, yylval.line); break;
			case TYPEDEF: printf("%s\t%d\tTYPEDEF\n", yylval.file, yylval.line); break;
			case UNION: printf("%s\t%d\tUNION\n", yylval.file, yylval.line); break;
			case UNSIGNED: printf("%s\t%d\tUNSIGNED\n", yylval.file, yylval.line); break;
			case VOID: printf("%s\t%d\tVOID\n", yylval.file, yylval.line); break;
			case VOLATILE: printf("%s\t%d\tVOLTATILE\n", yylval.file, yylval.line); break;
			case WHILE: printf("%s\t%d\tWHILE\n", yylval.file, yylval.line); break;
			case _BOOL: printf("%s\t%d\t_BOOL\n", yylval.file, yylval.line); break;
			case _COMPLEX: printf("%s\t%d\t_COMPLEX\n", yylval.file, yylval.line); break;
			case _IMAGINARY: printf("%s\t%d\t_IMAGINARY\n", yylval.file, yylval.line); break;
			
			
			case IDENT: printf("%s\t%d\tIDENT\t%s\n", yylval.file, yylval.line, yylval.value.string_val); break;
			
			//TODO - format appropriately according to type
			case NUMBER: printf("%s\t%d\tNUMBER\t%lld\n", yylval.file, yylval.line, yylval.value.num_val.llint_val); break;
		}
	}
	printf("EOF\n");
	return 0;
}
