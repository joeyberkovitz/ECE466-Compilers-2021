#include "lexer_common.h"

extern int currLine;
extern char currFile[];
extern char *currStr;
extern unsigned int currStrLen;

int print_esc_str(FILE *stream, const struct printf_info *info, const void *const *args){
	const struct LexVal *lexVal;
	lexVal = (*((const union astnode **)(args[0])))->lexNode;
	
	int printLen = 0;
	for(int i = 0; i < lexVal->len - 1; i++){
		char printChar = lexVal->value.string_val[i];
		if(printChar == 34)
			printLen += fprintf(stream, "\\\"");
		else if(printChar == 39)
			printLen += fprintf(stream, "\\'");
		else if(printChar == 92)
			printLen += fprintf(stream, "\\\\");
		else if(printChar >= 32 && printChar <= 126)
			printLen += fprintf(stream, "%c", printChar);
		else if(printChar == '\0')
			printLen += fprintf(stream, "\\0");
		else if(printChar == '\a')
			printLen += fprintf(stream, "\\a");
		else if(printChar == '\b')
			printLen += fprintf(stream, "\\b");
		else if(printChar == '\f')
			printLen += fprintf(stream, "\\f");
		else if(printChar == '\n')
			printLen += fprintf(stream, "\\n");
		else if(printChar == '\r')
			printLen += fprintf(stream, "\\r");
		else if(printChar == '\t')
			printLen += fprintf(stream, "\\t");
		else if(printChar == '\v')
			printLen += fprintf(stream, "\\v");
		else
			printLen += fprintf(stream, "\\%03o", (unsigned char)printChar);
	}
	
	return printLen;
}

int print_esc_strinfo(const struct printf_info *info, size_t n, int *argtypes, int *size){
	if(n > 0)
		argtypes[0] = PA_POINTER;
	return 1;
}

void allocLex(){
    yylval.lexNode = malloc(sizeof(struct LexVal));
    yylval.lexNode->type = NODE_LEX;
}

void processLine(){
    allocLex();
	yylval.lexNode->file = currFile;
	yylval.lexNode->line = currLine;
}

void setStr(union astnode val, char *txt, size_t len){
	val.lexNode->value.string_val = malloc(len+1);
	
	if(val.lexNode->value.string_val == NULL){
		fprintf(stderr, "Failed to allocate pointer for LexVal string");
		return;
	}
	
	strncpy(val.lexNode->value.string_val, txt, len);
}

void printWarning(union astnode val, char *txt, int orig_flags){
	if(orig_flags != val.lexNode->flags)
		fprintf(stderr, "%s:%d: Warning: Integer value %s overflowed specified type\n", val.lexNode->file, val.lexNode->line, txt);
}

void setInt(union astnode val, char *txt, int flags, int base){
	errno = 0;
	unsigned long long int num = strtoull(txt, NULL, base);
	val.lexNode->value.num_val.integer_val = num;

	//Check table in 6.4.4.1 for order of type progression	
	if(errno != ERANGE){
		if(hasFlag(flags, int_type) && num <= INT_MAX){
			val.lexNode->flags = int_type;
			return;
		}

		if((hasFlag(flags, uint_type) || (hasFlag(flags, int_type) && base != 10)) && !(hasFlag(flags, lint_type) || hasFlag(flags, llint_type)) && num <= UINT_MAX){
			val.lexNode->flags = uint_type;
			printWarning(val, txt, flags);
			return;
		}

		if((hasFlag(flags, lint_type) || hasFlag(flags, int_type)) && !hasFlag(flags, ulint_type) && num <= LONG_MAX){
			val.lexNode->flags = lint_type;
			printWarning(val, txt, flags);
			return;
		}

		if((hasFlag(flags, ulint_type) || hasFlag(flags, uint_type) || ((hasFlag(flags, int_type) || hasFlag(flags, lint_type)) && base != 10)) && !hasFlag(flags, llint_type) && num <= ULONG_MAX){
			val.lexNode->flags = ulint_type;
			printWarning(val, txt, flags);
			return;
		}
	}
	
	if((hasFlag(flags, llint_type) || hasFlag(flags, int_type) || hasFlag(flags, lint_type) || (hasFlag(flags, uint_type) && base != 10)) && !(hasFlag(flags, uint_type) && (base == 10 || hasFlag(flags, llint_type)))){
		if(num <= LLONG_MAX){
			val.lexNode->flags = llint_type;
			printWarning(val, txt, flags);
			return;
		}
		else if(base == 10){
			val.lexNode->value.num_val.integer_val = LLONG_MAX;
			val.lexNode->flags = llint_type;
			printWarning(val, txt, flags);
			fprintf(stderr, "%s:%d: Warning: Integer value %s out of range of type long long int\n", val.lexNode->file, val.lexNode->line, txt);
			return;
		}
	}

	val.lexNode->flags = ullint_type;
	printWarning(val, txt, flags);
	if(errno == ERANGE)
		fprintf(stderr, "%s:%d: Warning: Integer value %s out of range of type unsigned long long int\n", val.lexNode->file, val.lexNode->line, txt);
	return;
}

void setFloat(union astnode val, char *txt, int flags){
	if(hasFlag(flags, float_type)){
		val.lexNode->value.num_val.float_val = strtof(txt, NULL);
		val.lexNode->flags = float_type;
	}
	else if(hasFlag(flags, double_type)){
		val.lexNode->value.num_val.double_val = strtod(txt, NULL);
		val.lexNode->flags = double_type;
	}
	else{
		val.lexNode->value.num_val.ldouble_val = strtold(txt, NULL);
		val.lexNode->flags = ldouble_type;
	}
}

void resetString(){
	currStr = NULL;
	currStrLen = 0;
}

void initString(){
	currStr = malloc(1);
	currStrLen = 1;
}

void addChars(char *txt, size_t len, int flags){
	int startPos = currStrLen-1; //Always start new string at length-1
	if(hasFlag(flags, flag_octal) || hasFlag(flags, flag_hex))
		currStrLen += 1;
	else
		currStrLen += len;
	
	char *newPtr = (char*) realloc(currStr, currStrLen);
	if(newPtr == NULL){
		fprintf(stderr, "Failed to realloc string pointer for string or character literal processing");
		return;
	}
	
	currStr = newPtr;
	
	//If no flags, just add chars to string
	if(flags == 0){
		strncpy(currStr + startPos, txt, len);
	}
	else if(hasFlag(flags, flag_escaped)){
		//Process single escaped character
		if(txt[0] == '\'' || txt[0] == '"' || txt[0] == '?' || txt[0] == '\\')
			currStr[startPos] = txt[0];
		else if(txt[0] == 'a')
			currStr[startPos] = '\a';
		else if(txt[0] == 'b')
			currStr[startPos] = '\b';
		else if(txt[0] == 'f')
			currStr[startPos] = '\f';
		else if(txt[0] == 'n')
			currStr[startPos] = '\n';
		else if(txt[0] == 'r')
			currStr[startPos] = '\r';
		else if(txt[0] == 't')
			currStr[startPos] = '\t';
		else if(txt[0] == 'v')
			currStr[startPos] = '\v';
		
	}
	else if(hasFlag(flags, flag_octal) || hasFlag(flags, flag_hex)){
		//Process escaped number
		int base = hasFlag(flags, flag_octal) ? 8 : 16;
		long escVal = strtol(txt, NULL, base);
		
		//Clip to single signed byte
		if(escVal > 255){
			escVal = 255;
			if(base == 16)
				fprintf(stderr, "%s:%d:Warning:Hex escape sequence \\x%s out of range\n", currFile, currLine, txt);
			else if(base == 8)
				fprintf(stderr, "%s:%d:Warning:Octal escape sequence \\%s out of range\n", currFile, currLine, txt);
		}
		//printf("Escaped char val: %ld, orig str: %s, base: %d, flags: %d\n", escVal, txt, base, flags);
		
		currStr[startPos] = (char)escVal;
	}
}

void endString(union astnode val){
	val.lexNode->value.string_val = currStr;
	val.lexNode->len = currStrLen;
	resetString();
}

void endChar(union astnode val){
	if(currStrLen > 2){
		fprintf(stderr, "%s:%d:Warning:Unsupported multibyte character literal truncated to first byte\n", currFile, currLine);
		currStr = realloc(currStr, 2);
		currStr[1] = '\0';
	}
	val.lexNode->value.string_val = currStr;
	val.lexNode->len = 2;
	resetString();
}


void printLex(union astnode node, int tokType){
    switch(tokType) {
        //Keywords
        case AUTO:
            printf("%s\t%d\tAUTO\n", node.lexNode->file, node.lexNode->line);
            break;
        case BREAK:
            printf("%s\t%d\tBREAK\n", node.lexNode->file, node.lexNode->line);
            break;
        case CASE:
            printf("%s\t%d\tCASE\n", node.lexNode->file, node.lexNode->line);
            break;
        case CHAR:
            printf("%s\t%d\tCHAR\n", node.lexNode->file, node.lexNode->line);
            break;
        case CONST:
            printf("%s\t%d\tCONST\n", node.lexNode->file, node.lexNode->line);
            break;
        case CONTINUE:
            printf("%s\t%d\tCONTINUE\n", node.lexNode->file, node.lexNode->line);
            break;
        case DEFAULT:
            printf("%s\t%d\tDEFAULT\n", node.lexNode->file, node.lexNode->line);
            break;
        case DO:
            printf("%s\t%d\tDO\n", node.lexNode->file, node.lexNode->line);
            break;
        case DOUBLE:
            printf("%s\t%d\tDOUBLE\n", node.lexNode->file, node.lexNode->line);
            break;
        case ELSE:
            printf("%s\t%d\tELSE\n", node.lexNode->file, node.lexNode->line);
            break;
        case ENUM:
            printf("%s\t%d\tENUM\n", node.lexNode->file, node.lexNode->line);
            break;
        case EXTERN:
            printf("%s\t%d\tEXTERN\n", node.lexNode->file, node.lexNode->line);
            break;
        case FLOAT:
            printf("%s\t%d\tFLOAT\n", node.lexNode->file, node.lexNode->line);
            break;
        case FOR:
            printf("%s\t%d\tFOR\n", node.lexNode->file, node.lexNode->line);
            break;
        case GOTO:
            printf("%s\t%d\tGOTO\n", node.lexNode->file, node.lexNode->line);
            break;
        case IF:
            printf("%s\t%d\tIF\n", node.lexNode->file, node.lexNode->line);
            break;
        case INLINE:
            printf("%s\t%d\tINLINE\n", node.lexNode->file, node.lexNode->line);
            break;
        case INT:
            printf("%s\t%d\tINT\n", node.lexNode->file, node.lexNode->line);
            break;
        case LONG:
            printf("%s\t%d\tLONG\n", node.lexNode->file, node.lexNode->line);
            break;
        case REGISTER:
            printf("%s\t%d\tREGISTER\n", node.lexNode->file, node.lexNode->line);
            break;
        case RESTRICT:
            printf("%s\t%d\tRESTRICT\n", node.lexNode->file, node.lexNode->line);
            break;
        case RETURN:
            printf("%s\t%d\tRETURN\n", node.lexNode->file, node.lexNode->line);
            break;
        case SHORT:
            printf("%s\t%d\tSHORT\n", node.lexNode->file, node.lexNode->line);
            break;
        case SIGNED:
            printf("%s\t%d\tSIGNED\n", node.lexNode->file, node.lexNode->line);
            break;
        case SIZEOF:
            printf("%s\t%d\tSIZEOF\n", node.lexNode->file, node.lexNode->line);
            break;
        case STATIC:
            printf("%s\t%d\tSTATIC\n", node.lexNode->file, node.lexNode->line);
            break;
        case STRUCT:
            printf("%s\t%d\tSTRUCT\n", node.lexNode->file, node.lexNode->line);
            break;
        case SWITCH:
            printf("%s\t%d\tSWITCH\n", node.lexNode->file, node.lexNode->line);
            break;
        case TYPEDEF:
            printf("%s\t%d\tTYPEDEF\n", node.lexNode->file, node.lexNode->line);
            break;
        case UNION:
            printf("%s\t%d\tUNION\n", node.lexNode->file, node.lexNode->line);
            break;
        case UNSIGNED:
            printf("%s\t%d\tUNSIGNED\n", node.lexNode->file, node.lexNode->line);
            break;
        case VOID:
            printf("%s\t%d\tVOID\n", node.lexNode->file, node.lexNode->line);
            break;
        case VOLATILE:
            printf("%s\t%d\tVOLATILE\n", node.lexNode->file, node.lexNode->line);
            break;
        case WHILE:
            printf("%s\t%d\tWHILE\n", node.lexNode->file, node.lexNode->line);
            break;
        case _BOOL:
            printf("%s\t%d\t_BOOL\n", node.lexNode->file, node.lexNode->line);
            break;
        case _COMPLEX:
            printf("%s\t%d\t_COMPLEX\n", node.lexNode->file, node.lexNode->line);
            break;
        case _IMAGINARY:
            printf("%s\t%d\t_IMAGINARY\n", node.lexNode->file, node.lexNode->line);
            break;

            //Identifiers
        case IDENT:
            printf("%s\t%d\tIDENT\t%s\n", node.lexNode->file, node.lexNode->line, node.lexNode->value.string_val);
            break;

        case STRING:
            printf("%s\t%d\tSTRING\t%S\n", node.lexNode->file, node.lexNode->line, (wchar_t *) &node);
            break;
        case CHARLIT:
            printf("%s\t%d\tCHARLIT\t%S\n", node.lexNode->file, node.lexNode->line, (wchar_t *) &node);
            break;

            //Integer/Float Constants
        case NUMBER:
            printf("%s\t%d\tNUMBER\t", node.lexNode->file, node.lexNode->line);
            switch (node.lexNode->flags) {
                case int_type:
                    printf("INTEGER\t%lld\tINT\n", node.lexNode->value.num_val.integer_val);
                    break;
                case uint_type:
                    printf("INTEGER\t%lld\tUNSIGNED,INT\n", node.lexNode->value.num_val.integer_val);
                    break;
                case lint_type:
                    printf("INTEGER\t%lld\tLONG\n", node.lexNode->value.num_val.integer_val);
                    break;
                case ulint_type:
                    printf("INTEGER\t%lld\tUNSIGNED,LONG\n", node.lexNode->value.num_val.integer_val);
                    break;
                case llint_type:
                    printf("INTEGER\t%lld\tLONGLONG\n", node.lexNode->value.num_val.integer_val);
                    break;
                case ullint_type:
                    printf("INTEGER\t%lld\tUNSIGNED,LONGLONG\n", node.lexNode->value.num_val.integer_val);
                    break;
                case float_type:
                    printf("REAL\t%g\tFLOAT\n", node.lexNode->value.num_val.float_val);
                    break;
                case double_type:
                    printf("REAL\t%g\tDOUBLE\n", node.lexNode->value.num_val.double_val);
                    break;
                case ldouble_type:
                    printf("REAL\t%Lg\tLONGDOUBLE\n", node.lexNode->value.num_val.ldouble_val);
                    break;
                default:
                    fprintf(stderr, "%s:%d: Error: An error has occurred\n", node.lexNode->file,
                            node.lexNode->line);
                    break;
            }

            break;

            //Punctuators
        case '[':
            printf("%s\t%d\t[\n", node.lexNode->file, node.lexNode->line);
            break;
        case ']':
            printf("%s\t%d\t]\n", node.lexNode->file, node.lexNode->line);
            break;
        case '(':
            printf("%s\t%d\t(\n", node.lexNode->file, node.lexNode->line);
            break;
        case ')':
            printf("%s\t%d\t)\n", node.lexNode->file, node.lexNode->line);
            break;
        case '{':
            printf("%s\t%d\t{\n", node.lexNode->file, node.lexNode->line);
            break;
        case '}':
            printf("%s\t%d\t}\n", node.lexNode->file, node.lexNode->line);
            break;
        case '.':
            printf("%s\t%d\t.\n", node.lexNode->file, node.lexNode->line);
            break;
        case INDSEL:
            printf("%s\t%d\tINDSEL\n", node.lexNode->file, node.lexNode->line);
            break;
        case PLUSPLUS:
            printf("%s\t%d\tPLUSPLUS\n", node.lexNode->file, node.lexNode->line);
            break;
        case MINUSMINUS:
            printf("%s\t%d\tMINUSMINUS\n", node.lexNode->file, node.lexNode->line);
            break;
        case '&':
            printf("%s\t%d\t&\n", node.lexNode->file, node.lexNode->line);
            break;
        case '*':
            printf("%s\t%d\t*\n", node.lexNode->file, node.lexNode->line);
            break;
        case '+':
            printf("%s\t%d\t+\n", node.lexNode->file, node.lexNode->line);
            break;
        case '-':
            printf("%s\t%d\t-\n", node.lexNode->file, node.lexNode->line);
            break;
        case '~':
            printf("%s\t%d\t~\n", node.lexNode->file, node.lexNode->line);
            break;
        case '!':
            printf("%s\t%d\t!\n", node.lexNode->file, node.lexNode->line);
            break;
        case '/':
            printf("%s\t%d\t/\n", node.lexNode->file, node.lexNode->line);
            break;
        case '%':
            printf("%s\t%d\t%%\n", node.lexNode->file, node.lexNode->line);
            break;
        case SHL:
            printf("%s\t%d\tSHL\n", node.lexNode->file, node.lexNode->line);
            break;
        case SHR:
            printf("%s\t%d\tSHR\n", node.lexNode->file, node.lexNode->line);
            break;
        case '<':
            printf("%s\t%d\t<\n", node.lexNode->file, node.lexNode->line);
            break;
        case '>':
            printf("%s\t%d\t>\n", node.lexNode->file, node.lexNode->line);
            break;
        case LTEQ:
            printf("%s\t%d\tLTEQ\n", node.lexNode->file, node.lexNode->line);
            break;
        case GTEQ:
            printf("%s\t%d\tGTEQ\n", node.lexNode->file, node.lexNode->line);
            break;
        case EQEQ:
            printf("%s\t%d\tEQEQ\n", node.lexNode->file, node.lexNode->line);
            break;
        case NOTEQ:
            printf("%s\t%d\tNOTEQ\n", node.lexNode->file, node.lexNode->line);
            break;
        case '^':
            printf("%s\t%d\t^\n", node.lexNode->file, node.lexNode->line);
            break;
        case '|':
            printf("%s\t%d\t|\n", node.lexNode->file, node.lexNode->line);
            break;
        case LOGAND:
            printf("%s\t%d\tLOGAND\n", node.lexNode->file, node.lexNode->line);
            break;
        case LOGOR:
            printf("%s\t%d\tLOGOR\n", node.lexNode->file, node.lexNode->line);
            break;
        case '?':
            printf("%s\t%d\t?\n", node.lexNode->file, node.lexNode->line);
            break;
        case ':':
            printf("%s\t%d\t:\n", node.lexNode->file, node.lexNode->line);
            break;
        case ';':
            printf("%s\t%d\t;\n", node.lexNode->file, node.lexNode->line);
            break;
        case ELLIPSIS:
            printf("%s\t%d\tELLIPSIS\n", node.lexNode->file, node.lexNode->line);
            break;
        case '=':
            printf("%s\t%d\t=\n", node.lexNode->file, node.lexNode->line);
            break;
        case TIMESEQ:
            printf("%s\t%d\tTIMESEQ\n", node.lexNode->file, node.lexNode->line);
            break;
        case DIVEQ:
            printf("%s\t%d\tDIVEQ\n", node.lexNode->file, node.lexNode->line);
            break;
        case MODEQ:
            printf("%s\t%d\tMODEQ\n", node.lexNode->file, node.lexNode->line);
            break;
        case PLUSEQ:
            printf("%s\t%d\tPLUSEQ\n", node.lexNode->file, node.lexNode->line);
            break;
        case MINUSEQ:
            printf("%s\t%d\tMINUSEQ\n", node.lexNode->file, node.lexNode->line);
            break;
        case SHLEQ:
            printf("%s\t%d\tSHLEQ\n", node.lexNode->file, node.lexNode->line);
            break;
        case SHREQ:
            printf("%s\t%d\tSHREQ\n", node.lexNode->file, node.lexNode->line);
            break;
        case ANDEQ:
            printf("%s\t%d\tANDEQ\n", node.lexNode->file, node.lexNode->line);
            break;
        case XOREQ:
            printf("%s\t%d\tXOREQ\n", node.lexNode->file, node.lexNode->line);
            break;
        case OREQ:
            printf("%s\t%d\tOREQ\n", node.lexNode->file, node.lexNode->line);
            break;
        case ',':
            printf("%s\t%d\t,\n", node.lexNode->file, node.lexNode->line);
            break;
        case '#':
            printf("%s\t%d\t#\n", node.lexNode->file, node.lexNode->line);
            break;
        case OCTOCT:
            printf("%s\t%d\tOCTOCT\n", node.lexNode->file, node.lexNode->line);
            break;

            //EOF
        case TOKEOF:
            break;

        default:
            fprintf(stderr, "%s:%d: Error: An error has occurred\n", node.lexNode->file, node.lexNode->line);
            break;
    }
}