#include <stddef.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <errno.h>
#include <limits.h>

#define hasFlag(x, flag)	((x & flag) == flag)

typedef union {
	unsigned long long int integer_val;
	float float_val;
	double double_val;
	long double ldouble_val;
} NUMTYPE;

enum token_flags {
	int_type = 0x01,
	uint_type = 0x02,
	lint_type = 0x04,
	ulint_type = 0x04 | 0x02,
	llint_type = 0x08,
	ullint_type = 0x08 | 0x02,
	float_type = 0x10,
	double_type = 0x20,
	ldouble_type = 0x40
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

void printWarning(struct LexVal *val, char *txt, int orig_flags){
	if(orig_flags != val->flags)
		fprintf(stderr, "%s:%d: Warning: Integer value %s overflowed specified type\n", val->file, val->line, txt);
}

void setInt(struct LexVal *val, char *txt, int flags, int base){
	//TODO: include warning statements when type overflows
	errno = 0;
	unsigned long long int num = strtoull(txt, NULL, base);
	val->value.num_val.integer_val = num;
	
	if(errno != ERANGE){
		if(hasFlag(flags, int_type) && num <= INT_MAX){
			val->flags = int_type;
			return;
		}

		if((hasFlag(flags, uint_type) || (hasFlag(flags, int_type) && base != 10)) && !(hasFlag(flags, lint_type) || hasFlag(flags, llint_type)) && num <= UINT_MAX){
			val->flags = uint_type;
			printWarning(val, txt, flags);
			return;
		}

		if((hasFlag(flags, lint_type) || hasFlag(flags, int_type)) && !hasFlag(flags, ulint_type) && num <= LONG_MAX){
			val->flags = lint_type;
			printWarning(val, txt, flags);
			return;
		}

		if((hasFlag(flags, ulint_type) || hasFlag(flags, uint_type) || ((hasFlag(flags, int_type) || hasFlag(flags, lint_type)) && base != 10)) && !hasFlag(flags, llint_type) && num <= ULONG_MAX){
			val->flags = ulint_type;
			printWarning(val, txt, flags);
			return;
		}
	}
	
	if((hasFlag(flags, llint_type) || hasFlag(flags, int_type) || hasFlag(flags, lint_type) || (hasFlag(flags, uint_type) && base != 10)) && !(hasFlag(flags, uint_type) && (base == 10 || hasFlag(flags, llint_type)))){
		if(num <= LLONG_MAX){
			val->flags = llint_type;
			printWarning(val, txt, flags);
			return;
		}
		else if(base == 10){
			val->value.num_val.integer_val = LLONG_MAX;
			val->flags = llint_type;
			printWarning(val, txt, flags);
			fprintf(stderr, "%s:%d: Warning: Integer value %s out of range of type long long int\n", val->file, val->line, txt);
			return;
		}
	}

	val->flags = ullint_type;
	printWarning(val, txt, flags);
	if(errno == ERANGE)
		fprintf(stderr, "%s:%d: Warning: Integer value %s out of range of type unsigned long long int\n", val->file, val->line, txt);
	return;
}

void setFloat(struct LexVal *val, char *txt, int flags){
	if(hasFlag(flags, float_type)){
		val->value.num_val.float_val = strtof(txt, NULL);
		val->flags = float_type;
	}
	else if(hasFlag(flags, double_type)){
		val->value.num_val.double_val = strtod(txt, NULL);
		val->flags = double_type;
	}
	else{
		val->value.num_val.ldouble_val = strtold(txt, NULL);
		val->flags = ldouble_type;
	}
}
