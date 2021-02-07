#include "build/src/lexer/lexer.l.o.h"

int currLine = 1;
char currFile[256] = "<stdin>"; //Linux limit is 255
char *currStr = NULL;
unsigned int currStrLen = 0;


int main(int argc, char *argv[]) {
	if(argc > 1){
		yyin = fopen(argv[1], "r");
		if(!yyin){
			fprintf(stderr, "Error: Could not open file %s\n", argv[1]);
			return 1;
		}
		
		strncpy(currFile, argv[1], 255);
	}

	register_printf_specifier('S', print_esc_str, print_esc_strinfo);
	int t;
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
			case VOLATILE: printf("%s\t%d\tVOLATILE\n", yylval.file, yylval.line); break;
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
					case uint_type: printf("INTEGER\t%lld\tUNSIGNED,INT\n", yylval.value.num_val.integer_val); break;
					case lint_type:	printf("INTEGER\t%lld\tLONG\n", yylval.value.num_val.integer_val); break;
					case ulint_type: printf("INTEGER\t%lld\tUNSIGNED,LONG\n", yylval.value.num_val.integer_val); break;
					case llint_type: printf("INTEGER\t%lld\tLONGLONG\n", yylval.value.num_val.integer_val); break;
					case ullint_type: printf("INTEGER\t%lld\tUNSIGNED,LONGLONG\n", yylval.value.num_val.integer_val); break;
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

