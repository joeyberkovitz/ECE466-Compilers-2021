#include <stddef.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <errno.h>
#include <limits.h>

typedef union {
	long long_int_val;
	unsigned long int u_long_int_val;
	long long long_long_int_val;
	unsigned long long int u_long_long_int_val;
} NUMTYPE;

enum num_type {
	int_type,
	u_int_type,
	long_int_type,
	u_long_int_type,
	long_long_int_type,
	u_long_long_int_type
};

//TODO: change this to something more useful
typedef union {
	char *string_val;
	NUMTYPE num_val;
} YYSTYPE;

struct LexVal {
	char *file;
	int line;
	enum num_type type;
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

void setInt(struct LexVal *val, char *txt, int uflag, int lflag, int llflag, int base){
	//TODO: include warning statements when type overflows
	errno = 0;
	if(!((uflag && lflag) || llflag)){
		long int num = strtol(txt, NULL, base);
		if (errno != ERANGE){		
			if (!(lflag || uflag) && num <= INT_MAX){
				val->value.num_val.long_int_val = num;
				val->type = int_type;
				return;
			}
			
			if(!(uflag && num > UINT_MAX)){
				val->value.num_val.long_int_val = num;
				if(uflag || !(base == 10 || lflag || num > UINT_MAX))
					val->type = u_int_type;
				else
					val->type = long_int_type;

				return;
			}
		}
		else{
			if(base == 10)
				llflag = 1;
		}
	}
	
	errno = 0;
	if(!llflag){
		unsigned long int num = strtoul(txt, NULL, base);
		if(errno != ERANGE){
			val->value.num_val.u_long_int_val = num;
			val->type = u_long_int_type;
			return;
		}
	}

	errno = 0;
	if(!uflag){
		long long int num = strtoll(txt, NULL, base);
		if(errno != ERANGE || base == 10){
			val->value.num_val.long_long_int_val = num;
			val->type = long_long_int_type;
			return;
		}
	}

	unsigned long long int num = strtoull(txt, NULL, base);
	val->value.num_val.u_long_long_int_val = num;
	val->type = u_long_long_int_type;
	return;
}
