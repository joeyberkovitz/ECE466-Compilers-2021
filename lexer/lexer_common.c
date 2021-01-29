#include <stddef.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <errno.h>
#include <limits.h>
#include <printf.h>

#define hasFlag(x, flag)	((x & flag) == flag)

typedef union {
	long lint_val;
	unsigned long int ulint_val;
	long long llint_val;
	unsigned long long int ullint_val;
} NUMTYPE;

enum token_flags {
	int_type = 0x01,
	uint_type = 0x02,
	lint_type = 0x04,
	ulint_type = 0x04 & 0x02,
	llint_type = 0x08,
	ullint_type = 0x08 & 0x02,
	flag_escaped = 0x10, //For processing escaped characters appropriately
	flag_octal = 0x20,
	flag_hex = 0x40
};

//TODO: change this to something more useful
typedef union {
	char *string_val;
	NUMTYPE num_val;
} YYSTYPE;

struct LexVal {
	char *file;
	int line;
	int len;
	enum token_flags flags;
	YYSTYPE value;
};

struct LexVal yylval;

extern int currLine;
extern char currFile[];
extern char *currStr;
extern unsigned int currStrLen;

int print_esc_str(FILE *stream, const struct printf_info *info, const void *const *args){
	const struct LexVal *lexVal;
	lexVal = *((const struct LexVal **)(args[0]));
	
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

void processLine(struct LexVal *val){
	val->file = currFile;
	val->line = currLine;
}

void setStr(struct LexVal *val, char *txt, size_t len){
	val->value.string_val = malloc(len+1);
	
	if(val->value.string_val == NULL){
		fprintf(stderr, "Failed to allocate pointer for LexVal string");
		return;
	}
	
	strncpy(val->value.string_val, txt, len);
}

void setInt(struct LexVal *val, char *txt, int flags, int base){
	//TODO: include warning statements when type overflows
	errno = 0;
	if(!((hasFlag(flags, uint_type) && hasFlag(flags, lint_type)) || hasFlag(flags, llint_type))){
		long int num = strtol(txt, NULL, base);
		if (errno != ERANGE){		
			if (!(hasFlag(flags, lint_type) || hasFlag(flags, uint_type)) && num <= INT_MAX){
				val->value.num_val.lint_val = num;
				val->flags |= int_type;
				return;
			}
			
			if(!(hasFlag(flags, uint_type) && num > UINT_MAX)){
				val->value.num_val.lint_val = num;
				if(hasFlag(flags, uint_type) || !(base == 10 || hasFlag(flags, lint_type) || num > UINT_MAX))
					val->flags |= uint_type;
				else
					val->flags |= lint_type;

				return;
			}
		}
		else{
			if(base == 10)
				flags |= llint_type;
		}
	}
	
	errno = 0;
	if(!hasFlag(flags, llint_type)){
		unsigned long int num = strtoul(txt, NULL, base);
		if(errno != ERANGE){
			val->value.num_val.ulint_val = num;
			val->flags |= ulint_type;
			return;
		}
	}

	errno = 0;
	if(!hasFlag(flags, uint_type)){
		long long int num = strtoll(txt, NULL, base);
		if(errno != ERANGE || base == 10){
			val->value.num_val.llint_val = num;
			val->flags |= llint_type;
			return;
		}
	}

	unsigned long long int num = strtoull(txt, NULL, base);
	val->value.num_val.ullint_val = num;
	val->flags |= ullint_type;
	return;
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
		if(escVal > 255)
			escVal = 255;
		//printf("Escaped char val: %ld, orig str: %s, base: %d, flags: %d\n", escVal, txt, base, flags);
		
		currStr[startPos] = (char)escVal;
	}
}

void endString(struct LexVal *yylval){
	yylval->value.string_val = currStr;
	yylval->len = currStrLen;
	resetString();
}

void endChar(struct LexVal *yylval){
	if(currStrLen > 2){
		currStr = realloc(currStr, 2);
		currStr[1] = '\0';
	}
	yylval->value.string_val = currStr;
	yylval->len = 2;
	resetString();
}

