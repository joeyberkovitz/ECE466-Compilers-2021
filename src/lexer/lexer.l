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


	extern union astnode yylval;
	
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
auto				{processLine(); return AUTO;}
break				{processLine(); return BREAK;}
case				{processLine(); return CASE;}
char				{processLine(); return CHAR;}
const				{processLine(); return CONST;}
continue			{processLine(); return CONTINUE;}
default				{processLine(); return DEFAULT;}
do				{processLine(); return DO;}
double				{processLine(); return DOUBLE;}
else				{processLine(); return ELSE;}
enum				{processLine(); return ENUM;}
extern				{processLine(); return EXTERN;}
float				{processLine(); return FLOAT;}
for				{processLine(); return FOR;}
goto				{processLine(); return GOTO;}
if				{processLine(); return IF;}
inline				{processLine(); return INLINE;}
int				{processLine(); return INT;}
long				{processLine(); return LONG;}
register			{processLine(); return REGISTER;}
restrict			{processLine(); return RESTRICT;}
return				{processLine(); return RETURN;}
short				{processLine(); return SHORT;}
signed				{processLine(); return SIGNED;}
sizeof				{processLine(); return SIZEOF;}
static				{processLine(); return STATIC;}
struct				{processLine(); return STRUCT;}
switch				{processLine(); return SWITCH;}
typedef				{processLine(); return TYPEDEF;}
union				{processLine(); return UNION;}
unsigned			{processLine(); return UNSIGNED;}
void				{processLine(); return VOID;}
volatile			{processLine(); return VOLATILE;}
while				{processLine(); return WHILE;}
_Bool				{processLine(); return _BOOL;}
_Complex			{processLine(); return _COMPLEX;}
_Imaginary			{processLine(); return _IMAGINARY;}


	/* Identifiers - 6.4.2 */
{NONDIGIT}{IDCHARS}*	{processLine(); setStr(yylval, yytext, yyleng); return IDENT;}

	/* Constants - 6.4.4 */
	/* Integers - 6.4.4.1 */
{DECIMAL}					{int base = 10; processLine(); setInt(yylval, yytext, int_type, base); return NUMBER;}
{DECIMAL}[Uu]					{int base = 10; processLine(); setInt(yylval, yytext, uint_type, base); return NUMBER;}
{DECIMAL}[Ll]					{int base = 10; processLine(); setInt(yylval, yytext, lint_type, base); return NUMBER;}
{DECIMAL}{ULONG}				{int base = 10; processLine(); setInt(yylval, yytext, ulint_type, base); return NUMBER;}
{DECIMAL}{LONGLONG}				{int base = 10; processLine(); setInt(yylval, yytext, llint_type, base); return NUMBER;}
{DECIMAL}{ULONGLONG}				{int base = 10; processLine(); setInt(yylval, yytext, ullint_type, base); return NUMBER;}

{OCTAL}						{int base = 8; processLine(); setInt(yylval, yytext, int_type, base); return NUMBER;}
{OCTAL}[Uu]					{int base = 8; processLine(); setInt(yylval, yytext, uint_type, base); return NUMBER;}
{OCTAL}[Ll]					{int base = 8; processLine(); setInt(yylval, yytext, lint_type, base); return NUMBER;}
{OCTAL}{ULONG}					{int base = 8; processLine(); setInt(yylval, yytext, ulint_type, base); return NUMBER;}
{OCTAL}{LONGLONG}				{int base = 8; processLine(); setInt(yylval, yytext, llint_type, base); return NUMBER;}
{OCTAL}{ULONGLONG}				{int base = 8; processLine(); setInt(yylval, yytext, ullint_type, base); return NUMBER;}

{HEX}						{int base = 16; processLine(); setInt(yylval, yytext, int_type, base);; return NUMBER;}
{HEX}[Uu]					{int base = 16; processLine(); setInt(yylval, yytext, uint_type, base); return NUMBER;}
{HEX}[Ll]					{int base = 16; processLine(); setInt(yylval, yytext, lint_type, base); return NUMBER;}
{HEX}{ULONG}					{int base = 16; processLine(); setInt(yylval, yytext, ulint_type, base); return NUMBER;}
{HEX}{LONGLONG}					{int base = 16; processLine(); setInt(yylval, yytext, llint_type, base); return NUMBER;}
{HEX}{ULONGLONG}				{int base = 16; processLine(); setInt(yylval, yytext, ullint_type, base); return NUMBER;}

	/* Floats - 6.4.4.2 */
[0-9]*\.[0-9]+([Ee]{EXPONENT})?				|
[0-9]+(\.|\.?[Ee]{EXPONENT})				|
0[Xx][0-9A-Fa-f]*\.[0-9A-Fa-f]+[Pp]{EXPONENT}		|
0[Xx][0-9A-Fa-f]+\.?[Pp]{EXPONENT}			{processLine(); setFloat(yylval, yytext, double_type); return NUMBER;}

[0-9]*\.[0-9]+([Ee]{EXPONENT})?[Ff]			|
[0-9]+(\.|\.?[Ee]{EXPONENT})[Ff]			|
0[Xx][0-9A-Fa-f]*\.[0-9A-Fa-f]+[Pp]{EXPONENT}[Ff]	|
0[Xx][0-9A-Fa-f]+\.?[Pp]{EXPONENT}[Ff]			{processLine(); setFloat(yylval, yytext, float_type); return NUMBER;}

[0-9]*\.[0-9]+([Ee]{EXPONENT})?[Ll]			|
[0-9]+(\.|\.?[Ee]{EXPONENT})[Ll]			|
0[Xx][0-9A-Fa-f]*\.[0-9A-Fa-f]+[Pp]{EXPONENT}[Ll]	|
0[Xx][0-9A-Fa-f]+\.?[Pp]{EXPONENT}[Ll]			{processLine(); setFloat(yylval, yytext, ldouble_type); return NUMBER;}


	/* Character Constants - 6.4.4.4, String literals - 6.4.5 */
<INITIAL>\'												{BEGIN(CHAR_CONST); initString();}
<INITIAL>L\'											{BEGIN(WCHAR_CONST); initString();}
<INITIAL>\"												{BEGIN(STR_LIT); initString();}
<INITIAL>L\"											{BEGIN(WSTR_LIT); initString();}
<CHAR_CONST,WCHAR_CONST,STR_LIT,WSTR_LIT>[^\\'"\n<<EOF>>]+		{addChars(yytext, yyleng, 0);}
	/* Note: start at second char - ignore "\" */
<CHAR_CONST,WCHAR_CONST,STR_LIT,WSTR_LIT>{ESCCHAR}		{addChars(yytext+1, yyleng-1, flag_escaped);}
<CHAR_CONST,WCHAR_CONST>\"								{addChars("\"", 1, 0);}
<STR_LIT,WSTR_LIT>\'									{addChars("\'", 1, 0);}
<CHAR_CONST,WCHAR_CONST,STR_LIT,WSTR_LIT>\\[0-7]{1,3}	{addChars(yytext+1, yyleng-1, flag_octal);}
<CHAR_CONST,WCHAR_CONST,STR_LIT,WSTR_LIT>\\{HEXSTR}		{addChars(yytext+2, yyleng-2, flag_hex);}
<CHAR_CONST,WCHAR_CONST,STR_LIT,WSTR_LIT>{INVALIDESC}	{fprintf(stderr, "%s:%d: Warning: Invalid escape character: '%s'\n", currFile, currLine, yytext);}
<CHAR_CONST,WCHAR_CONST>[\n<<EOF>>]						{BEGIN(INITIAL); resetString(); fprintf(stderr, "%s:%d: Error: missing terminating ' character\n", currFile, currLine);}
<STR_LIT,WSTR_LIT>[\n<<EOF>>]                           {BEGIN(INITIAL); resetString(); fprintf(stderr, "%s:%d: Error: missing terminating \" character\n", currFile, currLine);}
<CHAR_CONST,WCHAR_CONST>\'								{BEGIN(INITIAL); if(currStrLen > 1) {processLine(); endChar(yylval); return CHARLIT;} resetString(); fprintf(stderr, "%s:%d: Error: invalid empty character literal\n", currFile, currLine);}
<STR_LIT,WSTR_LIT>\"									{BEGIN(INITIAL); processLine(); endString(yylval); return STRING;}


	/* Punctuators - 6.4.6 */
\[|<:				{processLine(); return '[';}
\]|:>				{processLine(); return ']';}
\(				{processLine(); return '(';}
\)				{processLine(); return ')';}
\{|<%				{processLine(); return '{';}
\}|%>				{processLine(); return '}';}
\.				{processLine(); return '.';}
\->				{processLine(); return INDSEL;}
\+\+				{processLine(); return PLUSPLUS;}
\-\-				{processLine(); return MINUSMINUS;}
&				{processLine(); return '&';}
\*				{processLine(); return '*';}
\+				{processLine(); return '+';}
\-				{processLine(); return '-';}
~				{processLine(); return '~';}
!				{processLine(); return '!';}
\/				{processLine(); return '/';}
%				{processLine(); return '%';}
\<\<				{processLine(); return SHL;}
>>				{processLine(); return SHR;}
\<				{processLine(); return '<';}
>				{processLine(); return '>';}
\<=				{processLine(); return LTEQ;}
>=				{processLine(); return GTEQ;}
==				{processLine(); return EQEQ;}
!=				{processLine(); return NOTEQ;}
\^				{processLine(); return '^';}
\|				{processLine(); return '|';}
&&				{processLine(); return LOGAND;}
\|\|				{processLine(); return LOGOR;}
\?				{processLine(); return '?';}
:				{processLine(); return ':';}
;				{processLine(); return ';';}
\.\.\.				{processLine(); return ELLIPSIS;}
=				{processLine(); return '=';}
\*=				{processLine(); return TIMESEQ;}
\/=				{processLine(); return DIVEQ;}
%=				{processLine(); return MODEQ;}
\+=				{processLine(); return PLUSEQ;}
\-=				{processLine(); return MINUSEQ;}
\<\<=				{processLine(); return SHLEQ;}
>>=				{processLine(); return SHREQ;}
&=				{processLine(); return ANDEQ;}
\^=				{processLine(); return XOREQ;}
\|=				{processLine(); return OREQ;}
,				{processLine(); return ',';}
#|%:				{processLine(); return '#';}
##|%:%:				{processLine(); return OCTOCT;}

\r?\n				{currLine++;}
<*>[ \t]			{;}
<<EOF>>				{return TOKEOF;}
.				{fprintf(stderr, "%s:%d: Error: unrecognized token '%s'\n", currFile, currLine, yytext);}

%%
	/* Code to run */