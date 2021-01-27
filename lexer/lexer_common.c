#include <stddef.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>

//TODO: change this to something more useful
typedef union {
	char *string_val;
	int int_val;
} YYSTYPE;

struct LexVal {
	char *file;
	int line;
	YYSTYPE value;
};

struct LexVal yylval;

extern int currLine;
extern char currFile[];

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