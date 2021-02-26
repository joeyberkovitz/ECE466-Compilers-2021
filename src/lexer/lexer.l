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

    //TODO: if setting sym here, should also set in places where RET macro isn't used
	#define RET(x) processLine();yylval.lexNode->sym = x;return x;
	#define RETINT(t,b) processLine();setInt(yylval, yytext, t, b);yylval.lexNode->sym = NUMBER;return NUMBER;
	#define RETFLT(t) processLine();setFloat(yylval, yytext, t);yylval.lexNode->sym = NUMBER;return NUMBER;

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
DECIMAL		[1-9]{DIGIT}*
OCTAL		0[0-7]*
EXPONENT	([+-]?{DIGIT}+)
HEXIT		[0-9A-Fa-f]
HEXSTR		[Xx]{HEXIT}+
HEX		0{HEXSTR}
ULONG		([Ll][Uu]|[Uu][Ll])
LONGLONG	(LL|ll)
ULONGLONG	((LL|ll)[Uu]|[Uu](LL|ll))
FLTDEC		{DIGIT}*\.{DIGIT}+([Ee]{EXPONENT})?
FLTDECEXP	{DIGIT}+(\.|\.?[Ee]{EXPONENT})
FLTHEX		0[Xx]{HEX}*\.{HEX}+[Pp]{EXPONENT}
FLTHEXEXP	{HEX}\.?[Pp]{EXPONENT}
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
auto				{RET(AUTO)}
break				{RET(BREAK)}
case				{RET(CASE)}
char				{RET(CHAR)}
const				{RET(CONST)}
continue			{RET(CONTINUE)}
default				{RET(DEFAULT)}
do				{RET(DO)}
double				{RET(DOUBLE)}
else				{RET(ELSE)}
enum				{RET(ENUM)}
extern				{RET(EXTERN)}
float				{RET(FLOAT)}
for				{RET(FOR)}
goto				{RET(GOTO)}
if				{RET(IF)}
inline				{RET(INLINE)}
int				{RET(INT)}
long				{RET(LONG)}
register			{RET(REGISTER)}
restrict			{RET(RESTRICT)}
return				{RET(RETURN)}
short				{RET(SHORT)}
signed				{RET(SIGNED)}
sizeof				{RET(SIZEOF)}
static				{RET(STATIC)}
struct				{RET(STRUCT)}
switch				{RET(SWITCH)}
typedef				{RET(TYPEDEF)}
union				{RET(UNION)}
unsigned			{RET(UNSIGNED)}
void				{RET(VOID)}
volatile			{RET(VOLATILE)}
while				{RET(WHILE)}
_Bool				{RET(_BOOL)}
_Complex			{RET(_COMPLEX)}
_Imaginary			{RET(_IMAGINARY)}


	/* Identifiers - 6.4.2 */
{NONDIGIT}{IDCHARS}*	{processLine(); setStr(yylval, yytext, yyleng); yylval.lexNode->sym = IDENT; return IDENT;}

	/* Constants - 6.4.4 */
	/* Integers - 6.4.4.1 */
{DECIMAL}					{RETINT(int_type,10)}
{DECIMAL}[Uu]					{RETINT(uint_type,10)}
{DECIMAL}[Ll]					{RETINT(lint_type,10)}
{DECIMAL}{ULONG}				{RETINT(ulint_type,10)}
{DECIMAL}{LONGLONG}				{RETINT(llint_type,10)}
{DECIMAL}{ULONGLONG}				{RETINT(ullint_type,10)}

{OCTAL}						{RETINT(int_type,8)}
{OCTAL}[Uu]					{RETINT(uint_type,8)}
{OCTAL}[Ll]					{RETINT(lint_type,8)}
{OCTAL}{ULONG}					{RETINT(ulint_type,8)}
{OCTAL}{LONGLONG}				{RETINT(llint_type,8)}
{OCTAL}{ULONGLONG}				{RETINT(ullint_type,8)}

{HEX}						{RETINT(int_type,16)}
{HEX}[Uu]					{RETINT(uint_type,16)}
{HEX}[Ll]					{RETINT(lint_type,16)}
{HEX}{ULONG}					{RETINT(ulint_type,16)}
{HEX}{LONGLONG}					{RETINT(llint_type,16)}
{HEX}{ULONGLONG}				{RETINT(ullint_type,16)}

	/* Floats - 6.4.4.2 */
{FLTDEC}				|
{FLTDECEXP}				|
{FLTHEX}				|
{FLTHEXEXP}				{RETFLT(double_type)}

{FLTDEC}[Ff]				|
{FLTDECEXP}[Ff]				|
{FLTHEX}[Ff]				|
{FLTHEXEXP}[Ff]				{RETFLT(float_type)}

{FLTDEC}[Ll]				|
{FLTDECEXP}[Ll]				|
{FLTHEX}[Ll]				|
{FLTHEXEXP}[Ll]				{RETFLT(ldouble_type)}


	/* Character Constants - 6.4.4.4, String literals - 6.4.5 */
<INITIAL>\'								{BEGIN(CHAR_CONST); initString();}
<INITIAL>L\'								{BEGIN(WCHAR_CONST); initString();}
<INITIAL>\"								{BEGIN(STR_LIT); initString();}
<INITIAL>L\"								{BEGIN(WSTR_LIT); initString();}
<CHAR_CONST,WCHAR_CONST,STR_LIT,WSTR_LIT>[^\\'"\n<<EOF>>]+		{addChars(yytext, yyleng, 0);}
	/* Note: start at second char - ignore "\" */
<CHAR_CONST,WCHAR_CONST,STR_LIT,WSTR_LIT>{ESCCHAR}			{addChars(yytext+1, yyleng-1, flag_escaped);}
<CHAR_CONST,WCHAR_CONST>\"						{addChars("\"", 1, 0);}
<STR_LIT,WSTR_LIT>\'							{addChars("\'", 1, 0);}
<CHAR_CONST,WCHAR_CONST,STR_LIT,WSTR_LIT>\\[0-7]{1,3}			{addChars(yytext+1, yyleng-1, flag_octal);}
<CHAR_CONST,WCHAR_CONST,STR_LIT,WSTR_LIT>\\{HEXSTR}			{addChars(yytext+2, yyleng-2, flag_hex);}
<CHAR_CONST,WCHAR_CONST,STR_LIT,WSTR_LIT>{INVALIDESC}			{fprintf(stderr, "%s:%d: Warning: Invalid escape character: '%s'\n", currFile, currLine, yytext);}
<CHAR_CONST,WCHAR_CONST>[\n<<EOF>>]					{BEGIN(INITIAL); resetString(); fprintf(stderr, "%s:%d: Error: missing terminating ' character\n", currFile, currLine);}
<STR_LIT,WSTR_LIT>[\n<<EOF>>]                           		{BEGIN(INITIAL); resetString(); fprintf(stderr, "%s:%d: Error: missing terminating \" character\n", currFile, currLine);}
<CHAR_CONST,WCHAR_CONST>\'						{BEGIN(INITIAL); if(currStrLen > 1) {processLine(); endChar(yylval); yylval.lexNode->sym = CHARLIT; return CHARLIT;} resetString(); fprintf(stderr, "%s:%d: Error: invalid empty character literal\n", currFile, currLine);}
<STR_LIT,WSTR_LIT>\"							{BEGIN(INITIAL); processLine(); endString(yylval); yylval.lexNode->sym = STRING; return STRING;}


	/* Punctuators - 6.4.6 */
\[|<:				{RET('[')}
\]|:>				{RET(']')}
\(				{RET('(')}
\)				{RET(')')}
\{|<%				{RET('{')}
\}|%>				{RET('}')}
\.				{RET('.')}
\->				{RET(INDSEL)}
\+\+				{RET(PLUSPLUS)}
\-\-				{RET(MINUSMINUS)}
&				{RET('&')}
\*				{RET('*')}
\+				{RET('+')}
\-				{RET('-')}
~				{RET('~')}
!				{RET('!')}
\/				{RET('/')}
%				{RET('%')}
\<\<				{RET(SHL)}
>>				{RET(SHR)}
\<				{RET('<')}
>				{RET('>')}
\<=				{RET(LTEQ)}
>=				{RET(GTEQ)}
==				{RET(EQEQ)}
!=				{RET(NOTEQ)}
\^				{RET('^')}
\|				{RET('|')}
&&				{RET(LOGAND)}
\|\|				{RET(LOGOR)}
\?				{RET('?')}
:				{RET(':')}
;				{RET(';')}
\.\.\.				{RET(ELLIPSIS)}
=				{RET('=')}
\*=				{RET(TIMESEQ)}
\/=				{RET(DIVEQ)}
%=				{RET(MODEQ)}
\+=				{RET(PLUSEQ)}
\-=				{RET(MINUSEQ)}
\<\<=				{RET(SHLEQ)}
>>=				{RET(SHREQ)}
&=				{RET(ANDEQ)}
\^=				{RET(XOREQ)}
\|=				{RET(OREQ)}
,				{RET(',')}

\r?\n				{currLine++;}
<*>[ \t]			{;}
<<EOF>>				{return TOKEOF;}
.				{fprintf(stderr, "%s:%d: Error: unrecognized token '%s'\n", currFile, currLine, yytext);}

%%
	/* Code to run */
