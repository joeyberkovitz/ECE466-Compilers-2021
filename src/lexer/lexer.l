%option noyywrap
	/* Imports required by lexer */
%top{
	#include "ltests/tokens-manual.h"
	//#include "lexer/ltests/tokens-manual.h"
	#include "parser/parser.y.o.h"
	#include "lexer/lexer_common.h"

	#include <stdio.h>
	#include <stdlib.h>
	#include <printf.h>


	extern struct LexVal yylval;
	
	extern int currLine;
	extern char currFile[];
	extern char *currStr;
	extern unsigned int currStrLen;
}
	/* Definitions */
DIGIT		[0-9]
NONDIGIT	[A-Za-z_]
IDCHARS		[0-9A-Za-z_]
DECIMAL		[1-9][0-9]*
OCTAL		0[0-7]*
EXPONENT	([+-]?[0-9]+)
HEX		0[Xx][0-9A-Fa-f]+
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

^\#				BEGIN(GCC_META);
<GCC_META>{DIGIT}+		{currLine = atoi(yytext);}
<GCC_META>["]			BEGIN(GCC_META_FILE);
<GCC_META_FILE>.+/["]		{strncpy(currFile, yytext, 255); BEGIN(GCC_META_FILE_END);}
<GCC_META_FILE_END>["]		BEGIN(GCC_META_FLAGS);
<GCC_META_FLAGS>{DIGIT}		{;}
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
for				{processLine(&yylval); return FOR;}
goto				{processLine(&yylval); return GOTO;}
if				{processLine(&yylval); return IF;}
inline				{processLine(&yylval); return INLINE;}
int				{processLine(&yylval); return INT;}
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
{DECIMAL}[Uu]					{int base = 10; processLine(&yylval); setInt(&yylval, yytext, uint_type, base); return NUMBER;}
{DECIMAL}[Ll]					{int base = 10; processLine(&yylval); setInt(&yylval, yytext, lint_type, base); return NUMBER;}
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
{HEX}[Uu]					{int base = 16; processLine(&yylval); setInt(&yylval, yytext, uint_type, base); return NUMBER;}
{HEX}[Ll]					{int base = 16; processLine(&yylval); setInt(&yylval, yytext, lint_type, base); return NUMBER;}
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
<CHAR_CONST,WCHAR_CONST,STR_LIT,WSTR_LIT>[^\\'"\n<<EOF>>]+		{addChars(yytext, yyleng, 0); processLine(&yylval);}
	/* Note: start at second char - ignore "\" */
<CHAR_CONST,WCHAR_CONST,STR_LIT,WSTR_LIT>{ESCCHAR}		{addChars(yytext+1, yyleng-1, flag_escaped);}
<CHAR_CONST,WCHAR_CONST>\"								{addChars("\"", 1, 0);}
<STR_LIT,WSTR_LIT>\'									{addChars("\'", 1, 0);}
<CHAR_CONST,WCHAR_CONST,STR_LIT,WSTR_LIT>\\[0-7]{1,3}	{addChars(yytext+1, yyleng-1, flag_octal);}
<CHAR_CONST,WCHAR_CONST,STR_LIT,WSTR_LIT>\\{HEXSTR}		{addChars(yytext+2, yyleng-2, flag_hex);}
<CHAR_CONST,WCHAR_CONST,STR_LIT,WSTR_LIT>{INVALIDESC}	{fprintf(stderr, "%s:%d: Warning: Invalid escape character: '%s'\n", currFile, currLine, yytext);}
<CHAR_CONST,WCHAR_CONST>[\n<<EOF>>]						{BEGIN(INITIAL); resetString(); fprintf(stderr, "%s:%d: Error: missing terminating ' character\n", currFile, currLine);}
<STR_LIT,WSTR_LIT>[\n<<EOF>>]                                             {BEGIN(INITIAL); resetString(); fprintf(stderr, "%s:%d: Error: missing terminating \" character\n", currFile, currLine);}
<CHAR_CONST,WCHAR_CONST>\'								{BEGIN(INITIAL); if(currStrLen > 1) {processLine(&yylval); endChar(&yylval); return CHARLIT;} resetString(); fprintf(stderr, "%s:%d: Error: invalid empty character literal\n", currFile, currLine);}
<STR_LIT,WSTR_LIT>\"									{BEGIN(INITIAL); processLine(&yylval); endString(&yylval); return STRING;}


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
.				{fprintf(stderr, "%s:%d: Error: unrecognized token '%s'\n", currFile, currLine, yytext);}

%%
	/* Code to run */