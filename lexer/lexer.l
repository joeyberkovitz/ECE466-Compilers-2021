%option noyywrap
	/* Imports required by lexer */
%top{
	#include "ltests/tokens-manual.h"
	#include "lexer_common.c"
	#include <stdio.h>
	#include <stdlib.h>
	#include <printf.h>
	
	extern struct LexVal yylval;
	
	int currLine = 1;
	char currFile[256] = "<stdin>"; //Linux limit is 255
	char *currStr = NULL;
	unsigned int currStrLen = 0;
}
	/* Definitions */
DIGIT		[0-9]
NONDIGIT	[A-Za-z_]
IDCHARS		[0-9A-Za-z_]
DECIMAL		[1-9][0-9]*
OCTAL		0[0-7]*
EXPONENT	([+-]?[0-9]+)
HEX			0[Xx][0-9A-Fa-f]+
HEXSTR		[Xx][0-9A-Fa-f]+
ULONG		([Ll][Uu]|[Uu][Ll])
LONGLONG	(LL|ll)
ULONGLONG	((LL|ll)[Uu]|[Uu](LL|ll))
LITCHARS	[ !#$-&(-\[\]-~]
ESCCHAR		\\['"?\\abfnrtv]
INVALIDESC	\\[^'"?\\abfnrtv]

	/* States */
%x GCC_META
%x GCC_META_FILE
%x GCC_META_FILE_END
%x GCC_META_FLAGS
%x CHAR_CONST
%x WCHAR_CONST
%x STR_LIT
%x WSTR_LIT
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
do				{processLine(&yylval); return DO;}
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
{DECIMAL}					{int base = 10; processLine(&yylval); setInt(&yylval, yytext, int_type, base); return NUMBER;}
{DECIMAL}[uU]					{int base = 10; processLine(&yylval); setInt(&yylval, yytext, uint_type, base); return NUMBER;}
{DECIMAL}[lL]					{int base = 10; processLine(&yylval); setInt(&yylval, yytext, lint_type, base); return NUMBER;}
{DECIMAL}{ULONG}				{int base = 10; processLine(&yylval); setInt(&yylval, yytext, ulint_type, base); return NUMBER;}
{DECIMAL}{LONGLONG}				{int base = 10; processLine(&yylval); setInt(&yylval, yytext, llint_type, base); return NUMBER;}
{DECIMAL}{ULONGLONG}				{int base = 10; processLine(&yylval); setInt(&yylval, yytext, ullint_type, base); return NUMBER;}

{OCTAL}						{int base = 8; processLine(&yylval); setInt(&yylval, yytext, int_type, base); return NUMBER;}
{OCTAL}[Uu]					{int base = 8; processLine(&yylval); setInt(&yylval, yytext, uint_type, base); return NUMBER;}
{OCTAL}[Ll]					{int base = 8; processLine(&yylval); setInt(&yylval, yytext, lint_type, base); return NUMBER;}
{OCTAL}{ULONG}					{int base = 8; processLine(&yylval); setInt(&yylval, yytext, ulint_type, base); return NUMBER;}
{OCTAL}{LONGLONG}				{int base = 8; processLine(&yylval); setInt(&yylval, yytext, llint_type, base); return NUMBER;}
{OCTAL}{ULONGLONG}				{int base = 8; processLine(&yylval); setInt(&yylval, yytext, ullint_type, base); return NUMBER;}

{HEX}						{int base = 16; processLine(&yylval); setInt(&yylval, yytext, int_type, base);; return NUMBER;}
{HEX}[uU]					{int base = 16; processLine(&yylval); setInt(&yylval, yytext, uint_type, base); return NUMBER;}
{HEX}[lL]					{int base = 16; processLine(&yylval); setInt(&yylval, yytext, lint_type, base); return NUMBER;}
{HEX}{ULONG}					{int base = 16; processLine(&yylval); setInt(&yylval, yytext, ulint_type, base); return NUMBER;}
{HEX}{LONGLONG}					{int base = 16; processLine(&yylval); setInt(&yylval, yytext, llint_type, base); return NUMBER;}
{HEX}{ULONGLONG}				{int base = 16; processLine(&yylval); setInt(&yylval, yytext, ullint_type, base); return NUMBER;}

	/* Floats - 6.4.4.2 */
[0-9]*\.[0-9]+([Ee]{EXPONENT})?				|
[0-9]+(\.|\.?[Ee]{EXPONENT})				|
0[Xx][0-9A-Fa-f]*\.[0-9A-Fa-f]+[Pp]{EXPONENT}		|
0[Xx][0-9A-Fa-f]+\.?[Pp]{EXPONENT}			{setFloat(&yylval, yytext, double_type); processLine(&yylval); return NUMBER;}

[0-9]*\.[0-9]+([Ee]{EXPONENT})?[Ff]			|
[0-9]+(\.|\.?[Ee]{EXPONENT})[Ff]			|
0[Xx][0-9A-Fa-f]*\.[0-9A-Fa-f]+[Pp]{EXPONENT}[Ff]	|
0[Xx][0-9A-Fa-f]+\.?[Pp]{EXPONENT}[Ff]			{setFloat(&yylval, yytext, float_type); processLine(&yylval); return NUMBER;}

[0-9]*\.[0-9]+([Ee]{EXPONENT})?[Ll]			|
[0-9]+(\.|\.?[Ee]{EXPONENT})[Ll]			|
0[Xx][0-9A-Fa-f]*\.[0-9A-Fa-f]+[Pp]{EXPONENT}[Ll]	|
0[Xx][0-9A-Fa-f]+\.?[Pp]{EXPONENT}[Ll]			{setFloat(&yylval, yytext, ldouble_type); processLine(&yylval); return NUMBER;}


	/* Character Constants - 6.4.4.4, String literals - 6.4.5 */
<INITIAL>\'												{BEGIN(CHAR_CONST); initString();}
<INITIAL>L\'											{BEGIN(WCHAR_CONST); initString();}
<INITIAL>\"												{BEGIN(STR_LIT); initString();}
<INITIAL>L\"											{BEGIN(WSTR_LIT); initString();}
<CHAR_CONST,WCHAR_CONST,STR_LIT,WSTR_LIT>[^\\'"]+		{addChars(yytext, yyleng, 0); processLine(&yylval);}
	/* Note: start at second char - ignore "\" */
<CHAR_CONST,WCHAR_CONST,STR_LIT,WSTR_LIT>{ESCCHAR}		{addChars(yytext+1, yyleng-1, flag_escaped); processLine(&yylval);}
<CHAR_CONST,WCHAR_CONST>\"								{addChars("\"", 1, 0); processLine(&yylval);}
<STR_LIT,WSTR_LIT>\'									{addChars("\'", 1, 0); processLine(&yylval);}
<CHAR_CONST,WCHAR_CONST,STR_LIT,WSTR_LIT>\\[0-7]{1,3}	{addChars(yytext+1, yyleng-1, flag_octal); processLine(&yylval);}
<CHAR_CONST,WCHAR_CONST,STR_LIT,WSTR_LIT>\\{HEXSTR}		{addChars(yytext+2, yyleng-2, flag_hex); processLine(&yylval);}
<CHAR_CONST,WCHAR_CONST,STR_LIT,WSTR_LIT>{INVALIDESC}	{fprintf(stderr, "Error	: Invalid escape character: '%s'\n", yytext);}
<CHAR_CONST,WCHAR_CONST>\'								{BEGIN(INITIAL); endChar(&yylval); return CHARLIT;}
<STR_LIT,WSTR_LIT>\"									{BEGIN(INITIAL); endString(&yylval); return STRING;}


	/* Punctuators - 6.4.6 */
\[|<:				{processLine(&yylval); return '[';}
\]|:>				{processLine(&yylval); return ']';}
\(				{processLine(&yylval); return '(';}
\)				{processLine(&yylval); return ')';}
\{|<%				{processLine(&yylval); return '{';}
\}|%>				{processLine(&yylval); return '}';}
\.				{processLine(&yylval); return '.';}
\->				{processLine(&yylval); return INDSEL;}
\+\+				{processLine(&yylval); return PLUSPLUS;}
\-\-				{processLine(&yylval); return MINUSMINUS;}
&				{processLine(&yylval); return '&';}
\*				{processLine(&yylval); return '*';}
\+				{processLine(&yylval); return '+';}
\-				{processLine(&yylval); return '-';}
~				{processLine(&yylval); return '~';}
!				{processLine(&yylval); return '!';}
\/				{processLine(&yylval); return '/';}
%				{processLine(&yylval); return '%';}
\<\<				{processLine(&yylval); return SHL;}
>>				{processLine(&yylval); return SHR;}
\<				{processLine(&yylval); return '<';}
>				{processLine(&yylval); return '>';}
\<=				{processLine(&yylval); return LTEQ;}
>=				{processLine(&yylval); return GTEQ;}
==				{processLine(&yylval); return EQEQ;}
!=				{processLine(&yylval); return NOTEQ;}
\^				{processLine(&yylval); return '^';}
\|				{processLine(&yylval); return '|';}
&&				{processLine(&yylval); return LOGAND;}
\|\|				{processLine(&yylval); return LOGOR;}
\?				{processLine(&yylval); return '?';}
:				{processLine(&yylval); return ':';}
;				{processLine(&yylval); return ';';}
\.\.\.				{processLine(&yylval); return ELLIPSIS;}
=				{processLine(&yylval); return '=';}
\*=				{processLine(&yylval); return TIMESEQ;}
\/=				{processLine(&yylval); return DIVEQ;}
%=				{processLine(&yylval); return MODEQ;}
\+=				{processLine(&yylval); return PLUSEQ;}
\-=				{processLine(&yylval); return MINUSEQ;}
\<\<=				{processLine(&yylval); return SHLEQ;}
>>=				{processLine(&yylval); return SHREQ;}
&=				{processLine(&yylval); return ANDEQ;}
\^=				{processLine(&yylval); return XOREQ;}
\|=				{processLine(&yylval); return OREQ;}
,				{processLine(&yylval); return ',';}
#|%:				{processLine(&yylval); return '#';}
##|%:%:				{processLine(&yylval); return OCTOCT;}

\r?\n				{currLine++;}
<*>[ \t]			{;}
<<EOF>>				{return TOKEOF;}
.				{fprintf(stderr, "%s:%d: Error: unrecognized character '%s'\n", currFile, currLine, yytext);}

%%
	/* Code to run */

int main() {
	register_printf_specifier('S', print_esc_str, print_esc_strinfo);
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
			
			//Identifiers
			case IDENT: printf("%s\t%d\tIDENT\t%s\n", yylval.file, yylval.line, yylval.value.string_val); break;
			
			case STRING: printf("%s\t%d\tSTRING\t%S\n", yylval.file, yylval.line, (wchar_t *)&yylval); break;
			case CHARLIT: printf("%s\t%d\tCHARLIT\t%S\n", yylval.file, yylval.line, (wchar_t *)&yylval); break;

			//Integer/Float Constants
			case NUMBER:
				printf("%s\t%d\tNUMBER\t", yylval.file, yylval.line);
				switch(yylval.flags){
					case int_type: printf("INTEGER\t%lld\tINT\n", yylval.value.num_val.integer_val); break;
					case uint_type: printf("INTEGER\t%lld\tUNSIGNED, INT\n", yylval.value.num_val.integer_val); break;
					case lint_type:	printf("INTEGER\t%lld\tLONG\n", yylval.value.num_val.integer_val); break;
					case ulint_type: printf("INTEGER\t%lld\tUNSIGNED, LONG\n", yylval.value.num_val.integer_val); break;
					case llint_type: printf("INTEGER\t%lld\tLONGLONG\n", yylval.value.num_val.integer_val); break;
					case ullint_type: printf("INTEGER\t%lld\tUNSIGNED, LONGLONG\n", yylval.value.num_val.integer_val); break;
					case float_type: printf("REAL\t%g\tFLOAT\n", yylval.value.num_val.float_val); break;
					case double_type: printf("REAL\t%g\tDOUBLE\n", yylval.value.num_val.double_val); break;
					case ldouble_type: printf("REAL\t%Lg\tLONGDOUBLE\n", yylval.value.num_val.ldouble_val); break;
					default: fprintf(stderr, "%s:%d: Error: An error has occurred\n", yylval.file, yylval.line); break;
				}

				break;

			//Punctuators
			case '[': printf("%s\t%d\t[\n", yylval.file, yylval.line); break;
			case ']': printf("%s\t%d\t]\n", yylval.file, yylval.line); break;
			case '(': printf("%s\t%d\t(\n", yylval.file, yylval.line); break;
			case ')': printf("%s\t%d\t)\n", yylval.file, yylval.line); break;
			case '{': printf("%s\t%d\t{\n", yylval.file, yylval.line); break;
			case '}': printf("%s\t%d\t}\n", yylval.file, yylval.line); break;
			case '.': printf("%s\t%d\t.\n", yylval.file, yylval.line); break;
			case INDSEL: printf("%s\t%d\tINDSEL\n", yylval.file, yylval.line); break;
			case PLUSPLUS: printf("%s\t%d\tPLUSPLUS\n", yylval.file, yylval.line); break;
			case MINUSMINUS: printf("%s\t%d\tMINUSMINUS\n", yylval.file, yylval.line); break;
			case '&': printf("%s\t%d\t&\n", yylval.file, yylval.line); break;
			case '*': printf("%s\t%d\t*\n", yylval.file, yylval.line); break;
			case '+': printf("%s\t%d\t+\n", yylval.file, yylval.line); break;
			case '-': printf("%s\t%d\t-\n", yylval.file, yylval.line); break;
			case '~': printf("%s\t%d\t~\n", yylval.file, yylval.line); break;
			case '!': printf("%s\t%d\t!\n", yylval.file, yylval.line); break;
			case '/': printf("%s\t%d\t/\n", yylval.file, yylval.line); break;
			case '%': printf("%s\t%d\t%%\n", yylval.file, yylval.line); break;
			case SHL: printf("%s\t%d\tSHL\n", yylval.file, yylval.line); break;
			case SHR: printf("%s\t%d\tSHR\n", yylval.file, yylval.line); break;
			case '<': printf("%s\t%d\t<\n", yylval.file, yylval.line); break;
			case '>': printf("%s\t%d\t>\n", yylval.file, yylval.line); break;
			case LTEQ: printf("%s\t%d\tLTEQ\n", yylval.file, yylval.line); break;
			case GTEQ: printf("%s\t%d\tGTEQ\n", yylval.file, yylval.line); break;
			case EQEQ: printf("%s\t%d\tEQEQ\n", yylval.file, yylval.line); break;
			case NOTEQ: printf("%s\t%d\tNOTEQ\n", yylval.file, yylval.line); break;
			case '^': printf("%s\t%d\t^\n", yylval.file, yylval.line); break;
			case '|': printf("%s\t%d\t|\n", yylval.file, yylval.line); break;
			case LOGAND: printf("%s\t%d\tLOGAND\n", yylval.file, yylval.line); break;
			case LOGOR: printf("%s\t%d\tLOGOR\n", yylval.file, yylval.line); break;
			case '?': printf("%s\t%d\t?\n", yylval.file, yylval.line); break;
			case ':': printf("%s\t%d\t:\n", yylval.file, yylval.line); break;
			case ';': printf("%s\t%d\t;\n", yylval.file, yylval.line); break;
			case ELLIPSIS: printf("%s\t%d\tELLIPSIS\n", yylval.file, yylval.line); break;
			case '=': printf("%s\t%d\t=\n", yylval.file, yylval.line); break;
			case TIMESEQ: printf("%s\t%d\tTIMESEQ\n", yylval.file, yylval.line); break;
			case DIVEQ: printf("%s\t%d\tDIVEQ\n", yylval.file, yylval.line); break;
			case MODEQ: printf("%s\t%d\tMODEQ\n", yylval.file, yylval.line); break;
			case PLUSEQ: printf("%s\t%d\tPLUSEQ\n", yylval.file, yylval.line); break;
			case MINUSEQ: printf("%s\t%d\tMINUSEQ\n", yylval.file, yylval.line); break;
			case SHLEQ: printf("%s\t%d\tSHLEQ\n", yylval.file, yylval.line); break;
			case SHREQ: printf("%s\t%d\tSHREQ\n", yylval.file, yylval.line); break;
			case ANDEQ: printf("%s\t%d\tANDEQ\n", yylval.file, yylval.line); break;
			case XOREQ: printf("%s\t%d\tXOREQ\n", yylval.file, yylval.line); break;
			case OREQ: printf("%s\t%d\tOREQ\n", yylval.file, yylval.line); break;
			case ',': printf("%s\t%d\t,\n", yylval.file, yylval.line); break;
			case '#': printf("%s\t%d\t#\n", yylval.file, yylval.line); break;
			case OCTOCT: printf("%s\t%d\tOCTOCT\n", yylval.file, yylval.line); break;
		
			//EOF
			case TOKEOF: break;

			default: fprintf(stderr, "%s:%d: Error: An error has occurred\n", yylval.file, yylval.line); break;
		}
	}
	printf("EOF\n");
	return 0;
}
