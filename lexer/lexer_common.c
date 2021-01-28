#include <stddef.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <errno.h>
#include <limits.h>

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
	ullint_type = 0x08 & 0x02
};

//TODO: change this to something more useful
typedef union {
	char *string_val;
	NUMTYPE num_val;
} YYSTYPE;

struct LexVal {
	char *file;
	int line;
	enum token_flags flags;
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
