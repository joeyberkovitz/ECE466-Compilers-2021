#ifndef LEXER_COMMON_H
#define LEXER_COMMON_H

#include <stddef.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <errno.h>
#include <limits.h>
#include <printf.h>

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
	ldouble_type = 0x40,
	flag_escaped = 0x80, //For processing escaped characters appropriately
	flag_octal = 0x100,
	flag_hex = 0x200
};

//TODO: change this to something more useful
typedef union {
	char *string_val;
	NUMTYPE num_val;
} LexVals;

struct LexVal {
	char *file;
	int line;
	int len;
	enum token_flags flags;
	LexVals value;
};

struct LexVal yylval;

int print_esc_str(FILE *stream, const struct printf_info *info, const void *const *args);

int print_esc_strinfo(const struct printf_info *info, size_t n, int *argtypes, int *size);

void processLine(struct LexVal *val);

void setStr(struct LexVal *val, char *txt, size_t len);

void printWarning(struct LexVal *val, char *txt, int orig_flags);

void setInt(struct LexVal *val, char *txt, int flags, int base);

void setFloat(struct LexVal *val, char *txt, int flags);

void resetString();

void initString();

void addChars(char *txt, size_t len, int flags);

void endString(struct LexVal *yylval);

void endChar(struct LexVal *yylval);

#endif